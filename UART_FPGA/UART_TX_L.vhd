library IEEE;
use IEEE.STD_LOGIC_1164.ALL; 

-------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------UART TX MODULE ---------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
entity uart_tx_module is

	generic ( c_clkfreq  : integer := 20_000_000                                                  ;		 --Internal clock freq
			  c_baudrate : integer := 115200                                                       ;		 --Communication speed 
			  c_stopbit  : integer := 1                                                             		 --Number of stop bit 
			);                                                             
																	       
	port   (                                                               
			-------------------------------------------------------------------------------------------------------------------------------                        
			-----------------------------------------------INPUTS--------------------------------------------------------------------------                        
			-------------------------------------------------------------------------------------------------------------------------------                        
			clk 		   : in std_logic                          	       						   ;	     --Clock input
			data_i         : in std_logic_vector(7 downto 0)               						   ;         --Data input 
			tx_start_i     : in std_logic                                  						   ;	     --Start command
			rst            : in std_logic := '0'                           						   ;         --State Reset button
																	       	        
			-------------------------------------------------------------------------------------------------------------------------------                                    
			-----------------------------------------------OUTPUTS-------------------------------------------------------------------------                                    
			-------------------------------------------------------------------------------------------------------------------------------                                    
			tx_o           : out std_logic 								                           ;	     --Data out
			tx_busy        : out std_logic                                                         ;         --Tx busy or not
			tx_done_tick_o : out std_logic  						                               ;         --All data sent tick
			busy_led       : out std_logic                                                                   --Busy led on the board (LD4)    
			);                                                                                               
																	                             	        
end uart_tx_module;                                                                                          
																	                             	        
architecture Behavioral of uart_tx_module is                                                                 
		
	type states is (S_IDLE, S_START, S_DATA, S_PARITY, S_STOP)                                     ; 	     --States contained in the state machine
	signal state            : states 						   := S_IDLE				           ;         --Initial state value is S_IDLE 
	
	constant c_bittimer_lmt : integer 						   := c_clkfreq / c_baudrate  	       ;         --Bit timer max value 
	constant c_stopbit_lmt	: integer 						   := c_clkfreq / c_baudrate           ;	     --Stop bit timer max value
	
	signal bittimer         : integer range 0 to c_stopbit_lmt :=  0                               ;	     --Bit timer signal
	signal bitcntr          : integer range 0 to 7 			   :=  0	                           ;		 --Bit counter signal
	signal parity           : std_logic                        := '1'							   ;         --Parity bit signal     
	signal shift_reg        : std_logic_vector(7 downto 0)     :=(others=>'0')                     ;		 --Shift register signal 
	signal parity_reg		: std_logic_vector(7 downto 0)	   :=(others=>'0')					   ;	     --Parity value register
	signal busy_led_status  : std_logic                         := '0'                             ;         --Busy led active / passive signal
	
begin	                                                                                           
																								   
	process (clk, rst, tx_start_i)                                                                                  
	
	begin                                                                                          
		
		if(rst = '1') then
			
				state 							 <= S_IDLE										   ;		 --If reset button is active, state is S_IDLE 
			
		end if;
																								   
		if rising_edge(clk) then                                                                   
			
			bittimer <= bittimer + 1															   ;		 --Bit timer value increase (+1)                                                              
																								   
			case state is                                                                          
																								   
			
			when S_IDLE =>                                                                         
																								   
				tx_o 			                 <= '1'										       ;		 --Tx output IDLE state (logic high)
			    tx_done_tick_o                   <= '0'                                            ;         --Data transmit complated tick logic low 
				parity      	                 <= '0'										       ;         --Parity bit reset
				bitcntr     	                 <=  0										       ;		 --Bit counter reset
				bittimer                         <=  0										       ;		 --Bit timer reset
			    shift_reg   	                 <= (others => '0')							       ; 	     --Shift register reset
				busy_led_status                  <= '0'                                            ;         --Busy led status passive
				tx_busy                          <= '0'                                            ;
																					       
				if ( tx_start_i = '1' ) then                                     
																						       
					state                        <= S_START 								       ;		 --Next state S_START					
					shift_reg 	                 <= data_i 									       ;		 --data input transferring to shift register					
					parity_reg                   <= data_i                                         ;		 --Parity calculation register
					bittimer                     <=  0										       ;		 --Bit timer reset
					tx_o    	                 <= '0'										       ;		 --Tx output logic low
					tx_busy                      <= '1'                                            ;		 --Data transmitter bus is busy
					busy_led_status              <= '1'                                            ;	     --Busy led active																	       
				
				end if;                                                                            
																							       
			
			when S_START =>                                                                        
																							       
				if (bittimer = c_bittimer_lmt - 1 ) then                                           
																							       
					state 				         <= S_DATA									       ;	     --Next state S_DATA
					tx_o 				         <= shift_reg(0)							       ;		 --Tx output is shift register's 0. index data
					shift_reg(7)		         <= shift_reg(0)							       ;		 --Data shifting and registering
					shift_reg(6 downto 0)        <= shift_reg (7 downto 1)			               ;		 --Data shifting and registering
					bittimer			         <= 0										       ;		 --Bit timer reset
				  
				else
				   
					tx_o                         <='0'                                             ;
																							       
				end if;                                                                            
																							                                                                       
																								   
			when S_DATA =>                                                                         
																								   
				if (bitcntr = 7 ) then                                                             
																								   
					if (bittimer = c_bittimer_lmt - 1 ) then                                       
					    state                    <= S_PARITY								       ;         --Next state S_PARITY
					    tx_o     				 <= parity								           ;		 --Tx output is parity value 
					    bitcntr                  <= 0									           ;         --Bit counter reset
					    bittimer                 <= 0									           ;		 --Bit timer reset 
					    shift_reg 				 <= (others => '0')						           ;         --Shift register reset
					end if;                                                                        
				
				else                                                                               
					
					if (bittimer = c_bittimer_lmt - 1 ) then                                       
																								   
						tx_o 					<= shift_reg(0)								       ;	     --Tx output is shift register's 0. index data
						shift_reg(6 downto 0)   <= shift_reg(7 downto 1)					       ; 	     --Data shifting 
						
						bitcntr                 <= bitcntr + 1								       ; 		 --Increase the bit counter value
					    bittimer                <= 0                                               ;         --Bit timer reset
					    
						parity <= '1'  xor parity_reg(0) xor parity_reg(1) xor parity_reg(2)        		 --Parity bit calculation
						xor parity_reg(3)xor parity_reg(4)xor parity_reg(5)xor parity_reg(6)        
						xor parity_reg(7)													       ;		 
					
					end if;												                           
				end if;		                                                                       
																							                                                                                  
																							       
			when S_PARITY =>                                                                       
																						       
				if (bittimer = c_bittimer_lmt - 1) then                                            
																							       
					state 						<= S_STOP 									       ;		 --Next state S_STOP 
					tx_o						<= '1'   									       ;		 --Tx output is parity value 
					bittimer  					<=  0										       ;		 --Bit timer reset
																							       
			    else
			    
			        tx_o <= parity;
				
				
				end if;                                                                            
																							       
																							       
			when S_STOP =>                                                                         
						
				
																						       
				if (bittimer = c_bittimer_lmt - 1) then                                            
																							       
			        tx_done_tick_o 				<= '1'                                             ;		 --All data sent  
					bittimer 					<=  0										       ;		 --Bittimer reset
					state						<= S_IDLE 									       ;		 --Next state S_IDLE		  
					tx_busy					    <= '0'											   ;		 --Tx bus is busy	
					
				else                                                                               
																							       
					tx_o						<= '1'										       ;		 --Tx output logic high				    
				
				end if;				
			end case;		
		end if;
	end process; 
	               
	               busy_led                     <= busy_led_status                                 ;         --Busy led equals to busy led status
	               	
end Behavioral; 