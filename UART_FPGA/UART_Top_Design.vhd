library IEEE;
use IEEE.STD_LOGIC_1164.ALL; 
use IEEE.STD_LOGIC_UNSIGNED.ALL;
-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART Top Level-----------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------

entity Top_Level is

generic (
			c_clkfreq    : integer 		 := 100_000_000											   ;	     --Internal clock freq
			c_baudrate   : integer 		 := 115200												   ;		 --Communication speed
			c_stopbit    : integer 		 := 1															     --number of stop bit 
		);					                                                                       
																								   
port 	(

-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------INPUTS------------------------------------------------------------------------------	
					                                                                       
			clk          : in  std_logic														   ;		 --clock input
			rx_i         : in  std_logic														   ;		 --UART rx input
			rst          : in  std_logic :='0'                                       			   ;         --State reset

-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------OUTPUTS------------------------------------------------------------------------------			
			
			tx_o         : out std_logic														   ;		 --UART tx output
			led			 : out std_logic                                            			   ;         --Under Board Pulse    (LD7)    
			err_led_p	 : out std_logic														   ;	  	 --Error led            (LD6)
			err_led_f	 : out std_logic														   ;	  	 --Error led            (LD6)
			busy_led     : out std_logic                                                           ;         --Busy led             (LD4)
		    wrong_val_led: out std_logic                                                                     --First value is false (LD5)
		
		);

end Top_Level;

architecture Behavioral of Top_Level is

-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Component Declaration----------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------

component UART_Module is

generic (
			c_clkfreq      : integer 	   := 100_000_000										   ;	     --Internal clock freq
			c_baudrate     : integer 	   := 115200   	      									   ;		 --Communication speed
			c_stopbit      : integer 	   := 1													  			 --number of stop bit 
		);		
		
port 	(	
			clk            : in  std_logic														   ;		 --clock input
            rst            : in  std_logic :='0'                                     			   ;         --state reset button
-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART TX port-------------------------------------------------------------------------------			
			
			tx_start_i     : in std_logic                                  						   ;	     --start command
			tx_done_tick_o : out std_logic							       	        			   ;		 --data out information tick
			data_i 		   : in std_logic_vector (7 downto 0 )  								   ;		 --Data input value 
            tx_busy        : out std_logic                                          			   ;         --tx busy or not
            busy_led       : out std_logic                                                         ;         --Tx bus busy information led 
-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART RX port-------------------------------------------------------------------------------

			data_o 		   : out std_logic_vector (7 downto 0 ) 								   ;		 --Data output value 
			rx_done_tick_o : out std_logic					    								   ;	     --Data out information tick
		    frame_err      : out std_logic 									        			   ;		 --Frame error flag
		    parity_err 	   : out std_logic                                          			   ;	     --Data isn't true	
		
-------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Physical signal----------------------------------------------------------------------------
			rx_i           : in  std_logic														   ;		 --UART rx input
			tx_o           : out std_logic														   		     --UART tx output			
		);

end component;

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------UART SIGNALS-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type states_rx is (S_IDLE_RX, S_ADD_RX, S_SUB_RX,S_MUL_RX,S_MUL_V2_RX, S_AND_RX,S_ERR_RX, S_OR_RX, S_RST_RX	);		 --UART Top Module States 
signal state_rx            : states_rx 						:= S_IDLE_RX 					       			 ;		 
																											 
type states_tx is (S_IDLE_TX, S_TRS_TX)								    		     			 ;		 --UART Top Module States 
signal state_tx            : states_tx					    := S_IDLE_TX 				           			 ;		 
																											 
signal data_i 		  	: std_logic_vector (7 downto 0 )                      					   			 ;		 --Data input value 
signal tx_start_i 	  	: std_logic 			            := '0'								   			 ;		 --Transmitter start command
signal tx_done_tick_o 	: std_logic                         := '0'								   			 ;		 --Transmitter compated tick
signal tx_busy          : std_logic                         := '0'                  			   			 ;         --Transmitter bus is busy or not
																											 
signal data_o 		  	: std_logic_vector (7 downto 0 )    := (others =>'0')					   			 ;		 --Data output value 
signal data_rx_o		: std_logic_vector (7 downto 0 )    := (others =>'0')					   			 ;		 --Data output with stop and parity bit
signal rx_done_tick_o 	: std_logic                         := '0'								   			 ;		 --Reciever complated tick
																											 
																											 
signal data_val_1		: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --First data value 
signal data_val_2       : std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --Second data value
signal data_add			: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;	     --Adder value record
signal data_sub			: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --Subtraction value record
signal data_mul         : std_logic_vector ( 31 downto 0  ) := (others =>'0')       			   			 ;		 --Multiplication value record
signal data_mul_1       : std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --Multiplication value first half
signal data_mul_2 		: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --Multiplication value second half
signal data_and			: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --And value record
signal data_or			: std_logic_vector ( 15 downto 0  ) := (others =>'0')					   			 ;		 --Or value record
signal data_buffer   	: std_logic_vector (5*8-1 downto 0) := (others =>'0')					   			 ;		 --Data latch signal and slider
																											 
signal state_adress_rx  : integer range 0 to 15				:=  0 								   			 ;		 --State aress record
signal state_adress_tx  : integer range 0 to 15				:=  0 								   			 ;		 --State aress record
																											 
signal frame_err        : std_logic                         := '0'                  			   			 ;         --Frame error detected
signal parity_err       : std_logic                         := '0'                  			   			 ;         --Parity error detected
																											 
signal led_status		: std_logic  						:= '0'								   			 ;		 --Under Board Pulse led active / passive signal
signal err_led_status_p	: std_logic							:= '0'								   			 ;		 --Error led active / passive signal
signal err_led_status_f	: std_logic							:= '0'								   			 ;		 --Error led active / passive signal
signal wrong_val_status : std_logic                         := '0'                                 			 ;         --First value truth information led active / passive signal			
signal led_cntr			: integer range 0 to 100_000_000	:=  0								   			 ;		 --Led control counter 
																											 
signal start_cntr       : integer range 0 to 20             :=  1                   			   			 ; 		 --Transmitter start command counter (for 1 clock per) 
signal data_cntr        : integer range 0 to 10				:=  0								   			 ;		 --Data out counter  
																											 
signal data_recieved    : integer range 0 to 2 				:=  0								   			 ;
signal data_transmitted : integer range 0 to 2				:=  0								   			 ;
signal reset 			: std_logic							:= '0'								   			 ;
signal wr_val			: std_logic							:= '0'								   			 ;
signal rx_buff_rst      : std_logic                         := '0'                                 			 ;
signal tx_reset         : integer range 0 to 2              :=  0                                            ;
signal trs_valid        : integer range 0 to 2              :=  0                                            ;
signal state_reset_fl   : integer range 0 to 2              :=  0                                            ;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
begin

-------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Component Instantiation--------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
i_uart_module : UART_Module 

generic map(
			c_clkfreq      => c_clkfreq															   ,		 --Clock frequency
			c_baudrate     => c_baudrate														   ,		 --Communication speed
			c_stopbit      => c_stopbit 			                                               			 --UART stop bit number
		   )					                                                                   
																								   
port map   (				                                                                       
			clk            => clk																   ,		 --Board clock frequency (100MHz)
            rst            => rst																   ,		 --Reset command
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART TX port-----------------------------------------------------------			
			
			tx_start_i     => tx_start_i											   			   ,		 --Transmitter start command
			tx_done_tick_o => tx_done_tick_o                                           			   ,		 --All data transmitted tick
			data_i 		   => data_i                                                   			   ,		 --Data input
            tx_busy        => tx_busy                                                  			   ,		 --Tx bus is busy or not
            busy_led       => busy_led                                                             ,         --Tx busy information led
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART RX port-----------------------------------------------------------

			data_o 		   => data_o															   ,		 --Data output	
			rx_done_tick_o => rx_done_tick_o                                        			   ,		 --All data recieved tick
		    frame_err      => frame_err                                             			   ,		 --Frame error (stop bit error)
		    parity_err 	   => parity_err                                            			   ,		 --Parity error (wrong data)
		
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------Physical signal--------------------------------------------------------
			rx_i           => rx_i 																   ,		 --Rx bus
			tx_o           => tx_o																			 --Tx bus
		   );
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
process (clk , rst)
begin

if(rising_edge(clk)) then


if (led_cntr = 100_000_000) then																		 --One pulse on the board per second
		
	led_cntr 		   <= 0																	   ;		 --Led counter reset
	led_status 		   <= not led_status													   ;		 --Pulse led is active (LD7)
	
else
	
	led_cntr   		   <= led_cntr + 1														   ;		 --Led counter increase (+1)
	led_status 		   <= led_status														   ;	     --Pulse led is passive (LD7)
		
end if;
	

if(parity_err = '1' or frame_err ='1') then
	
	state_rx			   <= S_ERR_RX																   ;		 --Next state is S_ERR (because parity or frame error)
	
end if;

if(data_transmitted = 1) then
    
    data_recieved <= 0;
    data_add   <= (others=>'0');
	data_mul   <= (others=>'0');
	data_mul_1 <= (others=>'0');
	data_mul_2 <= (others=>'0');
	data_sub   <= (others=>'0');
	data_or    <= (others=>'0');
    data_and   <= (others=>'0');
    state_rx   <=   S_IDLE_RX  ;
    tx_reset   <= 1;

end if;

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
case state_rx is

when S_ERR_RX  =>
			
	if(parity_err = '1') then
	
	    err_led_status_p		 <= '1'                   							       ;		 --Error led is active (LD7)
	    
	end if;
	
	if(frame_err = '1') then
	
	    err_led_status_f		 <= '1'                   						    	   ;		 --Error led is active (LD7)
	
	end if;
	
	if(rst = '1') then
		
		state_rx 		 <= S_IDLE_RX														   ;		 --Next state is S_IDLE
		reset            <= '1'                                                            ;
		err_led_status_p <= '0'                          								   ;		 --Error led is passive (LD7)
		wrong_val_status <= '0'                                                            ;
				
	end if;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
when S_RST_RX =>
	
	data_add   <= (others=>'0');
	data_mul   <= (others=>'0');
	data_mul_1 <= (others=>'0');
	data_mul_2 <= (others=>'0');
	data_sub   <= (others=>'0');
	data_or    <= (others=>'0');
    data_and   <= (others=>'0');
	state_rx   <= S_IDLE_RX    ;
    
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
when S_IDLE_RX =>
	
	if(rx_done_tick_o = '1') then
	   
	    tx_reset <= 0;
		data_buffer(7 downto 0) 					<= data_o;
		data_buffer(5*8-1 downto 1*8) 				<= data_buffer (4*8-1 downto 0*8); 
		data_cntr 									<= data_cntr + 1;
		
	end if;
	
	if (data_buffer(1*8-1 downto 0*8) /= x"A0" AND													 --Communication valid isn't true	    
		data_buffer(1*8-1 downto 0*8) /= x"B0" AND
		data_buffer(1*8-1 downto 0*8) /= x"C0" AND
	    data_buffer(1*8-1 downto 0*8) /= x"D0" AND
	    data_buffer(1*8-1 downto 0*8) /= x"E0" AND
		data_cntr = 1) then
						    
	    wrong_val_status <= '1'                                                            ;         --Communication first value wrong led active
	    data_cntr 		 <=  0															   ;
		data_buffer 	 <= (others => '0')												   ;
		
	end if;
	
	if( data_buffer(5*8-1 downto 4*8) = x"A0" AND data_cntr = 5) then							     --Adder command	
							
		data_val_1	<=data_buffer(2*8-1 downto 0*8)										   ;	     --Data 1 value 
		data_val_2	<=data_buffer(4*8-1 downto 2*8)										   ;	     --Data 2 value
		data_buffer <=(others => '0')                                           		   ;	     --Data buffer reset	
		state_rx	<= S_ADD_RX															   ;	     --New state is S_ADD
		data_cntr 	<= 0																   ;	     --Data counter reset
	    
	      
	end if;
	
	if( data_buffer(5*8-1 downto 4*8) = x"B0" AND data_cntr = 5 ) then 				  				 --Subtraction command
		
		data_val_1	<=data_buffer(2*8-1 downto 0*8)										   ;	     --Data 1 value 
		data_val_2	<=data_buffer(4*8-1 downto 2*8)										   ;	     --Data 2 value
		data_buffer <=(others => '0')                                           		   ;	     --Data buffer reset
		state_rx 	<= S_SUB_RX															   ;	     --New state is S_SUB
		data_cntr 	<= 0																   ;	     --Data counter reset
	    
			
	end if;
	
	if( data_buffer(5*8-1 downto 4*8) = x"C0" AND data_cntr = 5 ) then 				  				 --Multiplication command
		
		data_val_1	<=data_buffer(2*8-1 downto 0*8)										   ;	     --Data 1 value 
		data_val_2	<=data_buffer(4*8-1 downto 2*8)										   ;	     --Data 2 value
		data_buffer <=(others => '0')                                           		   ;	     --Data buffer reset
		state_rx 	<= S_MUL_RX															   ;	     --New state is S_MUL
		data_cntr	<= 0																   ; 	     --Data counter reset
        
	   
	end if;
	
	if( data_buffer(5*8-1 downto 4*8) = x"D0" AND data_cntr = 5 ) then 				 			     --And command
		
		data_val_1	<=data_buffer(2*8-1 downto 0*8)										   ;	     --Data 1 value 
		data_val_2	<=data_buffer(4*8-1 downto 2*8)										   ;	     --Data 2 value
		data_buffer <=(others => '0')                                           		   ;	     --Data buffer reset
		state_rx 	<= S_AND_RX 														   ;	     --New state is S_AND
		data_cntr	<= 0																   ;	     --Data counter reset
 	    
    

	end if;
	
	if( data_buffer(5*8-1 downto 4*8) = x"E0" AND data_cntr = 5 ) then 				  			     --Or command
	
		data_val_1	<=data_buffer(2*8-1 downto 0*8)										   ;	     --Data 1 value 
		data_val_2	<=data_buffer(4*8-1 downto 2*8)										   ;	     --Data 2 value
		data_buffer <=(others => '0')                                           		   ;	     --Data buffer reset
		state_rx 	<= S_OR_RX															   ;	     --New state is S_OR
		data_cntr	<= 0																   ;	     --Data counter reset
	   
	         
	end if;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
	when S_ADD_RX => 																		  				 --Adder State
			
		data_add	     <= data_val_1 + data_val_2											   ;	  	 --Data 1 value add data 2 value
		data_val_1       <= (others => '0')													   ;	  	 --Data 1 value reset
		data_val_2       <= (others => '0')													   ;	  	 --Data 2 value reset
		state_adress_rx  <= 1																   ;	  	 --State adress (S_ADD) is 1
		state_rx 	     <= S_IDLE_RX														   ;	     
		data_recieved    <= 1																   ;
		--tx_reset <= 0;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------		
	when S_SUB_RX => 																		  				 --Subtraction State
		
		if( data_val_2 > data_val_1) then
		
		     data_sub	<= data_val_2 - data_val_1											   ;	 	 --Data 2 value subtract data 1 value
		
		else 
		
	      	data_sub	<= data_val_1 - data_val_2                                  		   ;		 --Data 1 value subtract data 2 value
		
		end if;
		
		data_val_1       <= (others => '0')													   ;	     --Data 1 value reset
		data_val_2       <= (others => '0')													   ;	     --Data 2 value reset
		state_adress_rx  <= 3																   ;	     --State adress (S_SUB) is 2
		state_rx 	     <= S_IDLE_RX														   ;
		data_recieved    <= 1										   						   ; 
		--tx_reset <= 0;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
	when S_MUL_RX => 																		  		 	 	 --Multiplication State
		
		data_mul	    <= data_val_1 * data_val_2             								   ;	  	 --Data 1 value multiplicate data 2 value
		data_val_1      <= (others => '0')													   ;	  	 --Data 1 value reset
		data_val_2      <= (others => '0')													   ;	  	 --Data 2 value reset
		state_adress_rx <= 5																   ;	  	 --State adress (S_MUL) is 5
		state_rx		<= S_MUL_V2_RX														   ;	  	 --New State is S_MUL_V2
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------		
	when S_MUL_V2_RX =>																					 --Multiplication V2 State
	
		data_mul_1    <= data_mul (31 downto 16)									   		   ;		 --Data mul first half
		data_mul_2    <= data_mul (15 downto 0 )									    	   ;		 --Data mul second half
		data_mul      <= (others => '0')													   ;		 --Data mul value reset
		data_recieved <= 1																	   ;
		state_rx 	     <= S_IDLE_RX														   ;
		--tx_reset <= 0;		
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	when S_AND_RX => 																		  				 --AND State
	
		data_and	     <= data_val_1 and data_val_2										   ;	     --Data 1 value and data 2 value
		data_val_1       <= (others => '0')													   ;	     --Data 1 value reset
		data_val_2       <= (others => '0')													   ;	     --Data 2 value reset
		state_adress_rx  <= 9																   ;	     --State adress (S_AND) is 4
		state_rx 	     <= S_IDLE_RX														   ;
		data_recieved    <= 1																   ; 
		--tx_reset <= 0;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	when S_OR_RX  => 																		  				 --OR State
			
		data_or 	    <= data_val_1 or data_val_2											   ;	     --Data 1 value or data 2 value
		data_val_1      <= (others => '0')													   ;	     --Data 1 value reset
		data_val_2      <= (others => '0')													   ;	     --Data 2 value reset
		state_adress_rx <= 11																   ;	     --State adress (S_OR) is 5 
		state_rx 	    <= S_IDLE_RX														   ;
		data_recieved   <= 1																   ; 
		--tx_reset <= 0;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

end case;
end if;
end process;



process (clk, rst)
begin

if(rising_edge(clk)) then
  
  if(tx_reset = 1)then
    
    state_tx   <= S_IDLE_TX;
    data_transmitted <= 0;
    state_tx <= S_IDLE_TX;
    
  end if;
  
case state_tx is

when S_IDLE_TX =>
    
	if(data_recieved = 1 and tx_busy = '0') then
	   
		state_adress_tx       <= state_adress_rx											  ; 
		state_tx 			  <= S_TRS_TX													  ;
	   
	end if;
	
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
when S_TRS_TX =>

        if(tx_reset = 0) then
            
           if (state_adress_tx = 1) then
			
					data_i      <= data_add	(15 downto 8)										   ;	     --Data input is value_1 and value_2 add (first half)
			        
					if (start_cntr = 1 and data_transmitted = 0) then
                
						tx_start_i <= '1'														   ;		 --Tx start command active (logic high)
						start_cntr <=  0 														   ;		 --Tx start command counter reset
           
					else
						
						tx_start_i <= '0'														   ;		 --Tx start command passive (logic low)
            
					end if;
            
					if( tx_done_tick_o = '1') then
               
						start_cntr <= start_cntr + 1											   ;		 --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;		 --Tx start command passive (logic low)
						state_adress_tx<= 2														   ;		 --State adress (ADD transmitted) is 2
                   
					end if;
			end if;

			
			if (state_adress_tx = 2) then
			
				data_i	    <= data_add (7 downto 0) 											   ;	     --Data input is value_1 and value_2 add (second half) 
				    
				if (start_cntr = 1 and data_transmitted = 0) then
                
					tx_start_i <= '1'															   ;		 --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                            
				else                                                                                        
                                                                                                            
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                            
				end if;                                                                                     
                                                                                                            
				if(tx_done_tick_o = '1') then                                                               
		                                                                                                    
					start_cntr <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)
					data_transmitted	<= 1													   ;
					state_tx			<= S_IDLE_TX											   ;
                    state_adress_tx     <= 0;		
                  
				end if;	
			end if;
		-----------------------------------------------------------------------------------------------------------------------
		----------------------------------------------Subtraction Transmitted--------------------------------------------------	
			if (state_adress_tx = 3) then
			
					data_i      <= data_sub	(15 downto 8)										   ;		 --Data input is value_1 and value_2 subtract (first half)		 	     
			                                                                                                 
					if (start_cntr = 1 and data_transmitted = 0) then                                                                 
                                                                                                             
						tx_start_i <= '1'												           ;         --Tx start command active (logic high)
						start_cntr <=  0 														   ;         --Tx start command counter reset
                                                                                                             
					else                                                                                     
						                                                                                     
						tx_start_i <= '0' 														   ;         --Tx start command passive (logic low)
                                                                                                             
					end if;                                                                                  
                                                                                                             
					if( tx_done_tick_o = '1') then                                                           
                                                                                                             
						start_cntr <= start_cntr + 1											   ;         --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
						state_adress_tx<= 4														   ;         --State adress (SUB transmitted) is 2
                                                                                                             
					end if;                                                                                  
			end if;                                                                                          
                                                                                                             
			                                                                                                 
			if (state_adress_tx = 4) then                                                                       
			                                                                                                 
				data_i	    <= data_sub (7 downto 0) 											   ;	     --Data input is value_1 and value_2 subtract (second half)
				                                                                                             
				if (start_cntr = 1 and data_transmitted = 0) then                                                                     
                                                                                                             
					tx_start_i <= '1'															   ;         --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                             
				else                                                                                         
                                                                                                             
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                             
				end if;                                                                                      
                                                                                                             
				if(tx_done_tick_o = '1') then                                                                
		                                                                                                     
					start_cntr <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)		
					data_transmitted	<= 1													   ;
					state_tx			<= S_IDLE_TX											   ;
					state_adress_tx     <= 0;	
					
				end if;	
			end if;
			
		-----------------------------------------------------------------------------------------------------------------------
		----------------------------------------------Multiplication Transmitted-----------------------------------------------	
			if (state_adress_tx = 5) then
			
					data_i      <= data_mul_1 (15 downto 8)										   ;	     --Data input is value_1 and value_2 multiplicate (first quarter)
			                                                                                                 
					if (start_cntr = 1 and data_transmitted = 0) then                                                                 
                                                                                                             
						tx_start_i <= '1'														   ;         --Tx start command active (logic high)
						start_cntr <=  0 														   ;         --Tx start command counter reset
                                                                                                             
					else                                                                                     
						                                                                                     
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
                                                                                                             
					end if;                                                                                  
                                                                                                             
					if( tx_done_tick_o = '1') then                                                           
                                                                                                             
						start_cntr <= start_cntr + 1											   ;         --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
						state_adress_tx<= 6														   ;         --State adress (Multiplication V2 transmitted) is 6
                                                                                                             
					end if;                                                                                  
			end if;                                                                                          
                                                                                                             
			                                                                                                 
			if (state_adress_tx = 6) then                                                                       
			                                                                                                 
				data_i	    <= data_mul_1 (7 downto 0) 											   ;	     --Data input is value_1 and value_2 multiplicate (second quarter)
				                                                                                             
				if (start_cntr = 1 and data_transmitted = 0) then                                                                     
                                                                                                             
					tx_start_i <= '1'															   ;		 --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                             
				else                                                                                         
                                                                                                             
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                             
				end if;                                                                                      
                                                                                                             
				if(tx_done_tick_o = '1') then                                                                
		                                                                                                     
					start_cntr   <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)
					state_adress_tx <= 7 														   ;	     --State adress (Multiplication V2 transmitted) is 7		
				    tx_start_i   <= '0'                                                            ;         --Tx start command reset
				    
				end if;	
			end if;
		-----------------------------------------------------------------------------------------------------------------------
		---------------------------------------------MULTICIPLATION V2 Transmitted---------------------------------------------		
		if (state_adress_tx = 7) then
			
					data_i      <= data_mul_2 (15 downto 8)										   ;		 --Data input is value_1 and value_2 multiplicate (thirth quarter)	  
			                                                                                                 
					if (start_cntr = 1 and data_transmitted = 0) then                                                                 
                                                                                                             
						tx_start_i <= '1'														   ;         --Tx start command active (logic high)
						start_cntr <=  0 														   ;         --Tx start command counter reset
                                                                                                             
					else                                                                                     
						                                                                                     
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
                                                                                                             
					end if;                                                                                  
                                                                                                             
					if( tx_done_tick_o = '1') then                                                           
                                                                                                             
						start_cntr <= start_cntr + 1											   ;         --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
						state_adress_tx<= 8														   ;         --State adress (Multiplication V2 transmitted) is 8
                                                                                                             
					end if;                                                                                  
			end if;                                                                                          
                                                                                                             
			                                                                                                 
			if (state_adress_tx = 8) then                                                                       
			                                                                                                 
				data_i	    <= data_mul_2 (7 downto 0) 											   ;	     --Data input is value_1 and value_2 multiplicate (fourth quarter)
				                                                                                             
				if (start_cntr = 1 and data_transmitted = 0) then                                                                     
                                                                                                             
					tx_start_i <= '1' 															   ;         --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                             
				else                                                                                         
                                                                                                             
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                             
				end if;                                                                                      
                                                                                                             
				if(tx_done_tick_o = '1') then                                                                
		                                                                                                     
					start_cntr <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)
					data_transmitted	<= 1													   ;
					state_tx			<= S_IDLE_TX											   ;
				    state_adress_tx     <= 0;	
				
				end if;	
			end if;
		-----------------------------------------------------------------------------------------------------------------------
		------------------------------------------------------AND Transmitted--------------------------------------------------	
			if (state_adress_tx = 9) then
			
					data_i      <= data_and	(15 downto 8)										   ;		 --Data input is value_1 and value_2 AND gate (first half)	  
			                                                                                                 
					if (start_cntr = 1) then                                                                 
                                                                                                             
						tx_start_i <= '1'														   ;         --Tx start command active (logic high)
						start_cntr <=  0 														   ;         --Tx start command counter reset
                                                                                                             
					else                                                                                     
						                                                                                     
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
                                                                                                             
					end if;                                                                                  
                                                                                                             
					if( tx_done_tick_o = '1') then                                                           
                                                                                                             
						start_cntr <= start_cntr + 1                                               ;         --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
						state_adress_tx<= 10												       ;         --State adress (AND transmitted) is 10
                                                                                                             
					end if;                                                                                  
			end if;                                                                                          
                                                                                                             
			                                                                                                 
			if (state_adress_tx = 10) then                                                                      
			                                                                                                 
				data_i	    <= data_add (7 downto 0) 											   ;	     --Data input is value_1 and value_2 AND gate (second half)
				                                                                                             
				if (start_cntr = 1) then                                                                     
                                                                                                             
					tx_start_i <= '1'															   ;         --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                             
				else                                                                                         
                                                                                                             
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                             
				end if;                                                                                      
                                                                                                             
				if(tx_done_tick_o = '1') then                                                                
		                                                                                                     
					start_cntr <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)
					data_transmitted	<= 1													   ;
					state_tx			<= S_IDLE_TX											   ;
					state_adress_tx     <= 0;			
				
				end if;	
			end if;
			
		----------------------------------------------------------------------------------------------------------------------
		------------------------------------------------------OR Transmitted--------------------------------------------------	
			if (state_adress_tx = 11) then
			
					data_i      <= data_or	(15 downto 8)										   ;	 	 --Data input is value_1 and value_2 OR gate (first half)
			                                                                                                 
					if (start_cntr = 1) then                                                                 
                                                                                                             
						tx_start_i <= '1'														   ;         --Tx start command active (logic high)
						start_cntr <=  0 														   ;         --Tx start command counter reset
                                                                                                             
					else                                                                                     
						                                                                                     
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
                                                                                                             
					end if;                                                                                  
                                                                                                             
					if( tx_done_tick_o = '1') then                                                           
                                                                                                             
						start_cntr <= start_cntr + 1											   ;         --Tx start command counter increase (+1)
						tx_start_i <= '0'														   ;         --Tx start command passive (logic low)
						state_adress_tx<= 12												       ;         --State adress (OR transmitted) is 12
                                                                                                             
					end if;                                                                                  
			end if;                                                                                          
                                                                                                             
			                                                                                                 
			if (state_adress_tx = 12) then                                                                      
			                                                                                                 
				data_i	    <= data_or (7 downto 0) 											   ;         --Data input is value_1 and value_2 OR gate (second half)
				                                                                                             
				if (start_cntr = 1) then                                                                     
                                                                                                             
					tx_start_i <= '1'															   ;         --Tx start command active (logic high)
					start_cntr <=  0 															   ;         --Tx start command counter reset
                                                                                                             
				else                                                                                         
                                                                                                             
					tx_start_i <= '0'															   ;         --Tx start command passive (logic low)
                                                                                                             
				end if;                                                                                      
                                                                                                             
				if(tx_done_tick_o = '1') then                                                                
		                                                                                                      
					start_cntr <= start_cntr + 1                                    			   ;         --Tx start command counter increase (+1)
					data_transmitted	<= 1													   ;
					state_tx			<= S_IDLE_TX											   ;
					state_adress_tx     <= 0;				
				
				end if;	
			end if;
		end if;
        ---------------------------------------------------------------------------------------------------------------------- 
end case;
end if;
end process;
                        led 	      <= led_status												   ;         --Led value equals to led status
						err_led_p     <= err_led_status_p										   ;		 --Error led value equals to error led status
                        err_led_f     <= err_led_status_f                                          ;
                        wrong_val_led <= wrong_val_status                                          ;         --Wrong led value equals to wrong value status
end Behavioral;