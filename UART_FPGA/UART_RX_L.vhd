library IEEE;
use IEEE.STD_LOGIC_1164.ALL; 
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART RX MODULE --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
entity uart_rx_module is

	generic ( c_clkfreq           : integer := 20_000_000                          			   ;		 --Internal clock freq
			  c_baudrate          : integer := 115_200                                		  				 --Communication speed  
			);                    
						          
	port	(
			 ----------------------------------------------------------------------------------------------------------------------------                        
			 --------------------------------------------INPUTS--------------------------------------------------------------------------                        
			 ----------------------------------------------------------------------------------------------------------------------------    
			  clk  		          : in std_logic									               ;    	 --Clock input
			  rx_i 	              : in std_logic									               ;		 --Rx value input
			  rst                 : in std_logic := '0'                                            ;         --State reset button
	         ----------------------------------------------------------------------------------------------------------------------------                                    
			 --------------------------------------------OUTPUTS-------------------------------------------------------------------------                                    
			 ----------------------------------------------------------------------------------------------------------------------------        		                      
     		  data_o	          : out std_logic_vector (7 downto 0)				               ;		 --Data output
			  rx_done_tick_o 	  : out std_logic := '0'							               ;	     --Data out information tick
			  frame_err           : out std_logic := '0'							               ;		 --Frame error flag
			  parity_err 		  : out std_logic := '0'                                                      --Data isn't true			  
			  
			);                                                                                     
																					               
end uart_rx_module;                                                                                
																					               
architecture Behavioral of uart_rx_module is                                                       
																					               
type states is (S_IDLE, S_START, S_DATA, S_PARITY, S_STOP, S_OUT, S_RESET )			               ;		 --States contained in the state machine
signal state 		: states := S_IDLE												               ;		 --Initial state is S_IDLE
																					               
constant c_bittimer_lmt : integer := c_clkfreq / c_baudrate 						               ;	     --Bit timer max value
																					               
signal bittimer 	: integer range 0 to c_bittimer_lmt								               ;		 --Bit timer signal
signal bitcntr  	: integer range 0 to 10                                                        ;	     --Bit counter signal
signal shift_reg	: std_logic_vector (7 downto 0)	       						                   ;	 	 --Data register signal
signal parity   	: std_logic := '1'															   ;	     --Parity signal (odd)
signal parity_err_fl: std_logic := '0'												               ;		 --Parity error flag
signal frame_err_fl : std_logic := '0'                                                             ;	     --Frame error flag

begin

	process (clk, rx_i, rst)
	begin
		
		if(rst = '1') then
		
			state         <= S_IDLE															       ;
			parity_err    <=  '0'																   ;         --Parity error reset	
			parity_err_fl <=  '0'																   ;	     --Parity error flag falled
			frame_err     <=  '0'																   ;		 --Frame error reset
			frame_err_fl  <=  '0'                                                                  ;		 --Frame error flag falled
			
		end if;
		
		if(rising_edge(clk)) then

			bittimer <= bittimer + 1 															   ;		 --Bit timer value increase (+1) 

			case state is
			
			
			when S_IDLE => 																					 
			
				bitcntr					<=  0													   ;		 --Bit counter reset
				parity_err 				<= '0'													   ;	     --Parity error reset
				frame_err 				<= '0'													   ;		 --Frame error reset
				bittimer 				<=  0													   ;		 --Bit timer reset
				rx_done_tick_o          <= '0'                                                     ;		 --All data recieved tick reset
				shift_reg 				<= (others=>'0')										   ;		 --Shift register reset
				
																								   
				if ( rx_i = '0' ) then                                                              
																								   
					state 				<= S_START												   ; 		 --New state is S_IDLE
					bittimer			<=  0													   ;		 --Bit timer reset
				
					
				end if;                                                                            
																								   
																								   
			when S_START =>                                                                        
																								   
				if ( bittimer = c_bittimer_lmt/2-1) then 													 --Half bit waiting                                          
																								   
					state 				<= S_DATA												   ;		 --New state is S_DATA
					bittimer 			<=  0													   ;		 --Bit timer reset
					
				end if; 
			
			
			when S_DATA  =>
			
				if (bittimer = c_bittimer_lmt-1) then
				
					if ( bitcntr = 7) then
					    
					    shift_reg   <= rx_i & shift_reg (7 downto 1)                               ;	     --Data shifting and registering
						state       <= S_PARITY                                                    ;		 --New state is S_PARITY
						bitcntr     <=  0														   ;		 --Bit counter reset
						bittimer    <=  0                                                          ;		 --Bit timer reset
					
					else
					
						bitcntr 	<= bitcntr + 1												   ;		 --Bit counter increase (+1)
						bittimer	<=  0														   ;		 --Bit timer reset
						shift_reg   <= rx_i & shift_reg (7 downto 1)                               ;	     --Data shifting and registering
					
					end if;
				end if;
			
			
			when S_PARITY =>
					
					   parity   <= '1' xor shift_reg(1) xor shift_reg(2) xor shift_reg(3)					 --Parity bit calculation
									   xor shift_reg(4) xor shift_reg(5) xor shift_reg(6)
									   xor shift_reg(7) xor shift_reg(0)						   ;
						
				if (bittimer = c_bittimer_lmt-1) then
					
					if ( parity = rx_i ) then
					
						state 			<= S_STOP												   ;		 --New state is S_STOP
						parity_err 		<= '0'													   ;		 --Parity error disactive
						parity_err_fl   <= '0'                                                     ;		 --Parity error flag falled
						bittimer 		<=  0 													   ;		 --Bit timer reset
					    parity 			<= '0'       											   ;		 --Parity value reset
					   
					end if;                                                                        
																								   
					if ( parity /= rx_i) then                                              
																								   
						state   	 	<= S_OUT												   ;		 --New state is S_OUT
						parity_err		<= '1'													   ;		 --Parity error active
						parity_err_fl   <= '1'                                                     ;		 --Parity error flag raised
						bittimer  		<=  0													   ;		 --Bit timer reset
					    parity 			<= '0'		         									   ;		 --Parity value reset
					  
					end if;
				end if;
				
			
			when S_STOP  =>
				
        				
		
				if ( bittimer = c_bittimer_lmt-1) then
					
					if( rx_i = '1') then
				
						state 			<= S_OUT												   ;		 --New state is S_OUT
						parity_err 		<= '0'													   ;		 --Parity error is disactive
						parity_err_fl   <= '0'                                                     ;      	 --Parity error flag falled
						frame_err 		<= '0'													   ;		 --Frame error is disactive
						frame_err_fl    <= '0'                                                     ;		 --Frame error flag falled
						bittimer 		<=  0													   ;	 	 --Bit timer reset
					   
					end if;
				
					if( rx_i = '0') then
					
						state 			<= S_OUT												   ;		 --New state is S_OUT
						frame_err       <=  '1'													   ;	     --Frame error active
						frame_err_fl    <=  '1'                                                    ;		 --Frame error flag raised
						bittimer 		<=   0													   ;		 --Bit timer reset
					
					end if;
				end if;
			
			
			when S_OUT    =>
				
					if(parity_err_fl = '1' OR frame_err_fl = '1') then							   			 --If parity error or frame error is active 
				
						state 			<= S_RESET												   ;		 --New state is S_RESET
						rx_done_tick_o	<=  '0' 												   ;		 --Data recieved complated tick logic low 
						shift_reg       <= (others=>'0')                                           ;		 --Shift register total (with parity, start and stop bit)
																								   
					end if;                                                                        
																								   
					if (parity_err_fl = '0' AND frame_err_fl = '0') then  									 --If parity error or frame error is disactive                          
																								   																		   
						state 			<= S_IDLE												   ;		 --New state is S_IDLE
						rx_done_tick_o 	<= '1'													   ;		 --All data recieved tick logic high
			            data_o         	<= shift_reg			    							   ;		 --Data transfer			
						
					end if;
			
				
			
			when S_RESET    => 
				
			
				if (rst = '1') then																			 --Reset required
					
					state <= S_IDLE                                                                ;	     --New state is S_IDLE   
								
				end if;
			end case;
		end if;
	end process;
end Behavioral;