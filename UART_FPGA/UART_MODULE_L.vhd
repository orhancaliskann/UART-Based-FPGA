library IEEE;
use IEEE.STD_LOGIC_1164.ALL;  
----------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART Module-----------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

entity UART_Module is

generic (
			c_clkfreq      : integer := 20_000_000									;	      --Internal clock freq
			c_baudrate     : integer := 115200										;		  --Communication speed
			c_stopbit      : integer := 1													  --number of stop bit 
		);		
		
port 	(	
			clk            : in  std_logic											;		  --clock input
            rst            : in  std_logic:='0'                                     ;         --state reset button
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART TX port-----------------------------------------------------------			
			
			tx_start_i     : in std_logic                                  			;	      --start command
			tx_done_tick_o : out std_logic							       	        ;		  --data out information tick
			data_i 		   : in std_logic_vector (7 downto 0 )  					;		  --Data input value 
            tx_busy        : out std_logic                                          ;         --tx busy or not
            busy_led       : out std_logic                                          ;         --Tx bus busy information led
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------UART RX port-----------------------------------------------------------
            
			data_o 		   : out std_logic_vector (7 downto 0 ) 					;		  --Data output value 
			rx_done_tick_o : out std_logic					    					;	      --Data out information tick
		    frame_err      : out std_logic 									        ;		  --Frame error flag
		    parity_err 	   : out std_logic                                          ;	      --Data isn't true	
		
-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------Physical signal--------------------------------------------------------
			rx_i           : in  std_logic											;		  --UART rx input
			tx_o           : out std_logic													  --UART tx output			
		);

end UART_Module;

architecture Behavioral of UART_Module is

-----------------------------------------------------------------------------------------------------------------------
------------------------------------------------Component Declaration--------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
component uart_tx_module is

	generic ( c_clkfreq  : integer := 20_000_000                          			;		  --Internal clock freq
			  c_baudrate : integer := 115200                               			;		  --Communication speed 
			  c_stopbit  : integer := 1                                    			 		  --number of stop bit 
			);                                                             
																	       
	port   (                                                               
			-------------------------------------------------------------------------------------------------------------                        
			--------------------------------------------INPUTS-----------------------------------------------------------                        
			-------------------------------------------------------------------------------------------------------------                        
			clk 		   : in std_logic                          	       			;	      --clock input
			data_i         : in std_logic_vector(7 downto 0)               			;         --data input 
			tx_start_i     : in std_logic                                  			;	      --start command
			rst            : in std_logic := '0'                                    ;         --State Reset button													       	        
			-------------------------------------------------------------------------------------------------------------                                    
			--------------------------------------------OUTPUTS----------------------------------------------------------                                    
			-------------------------------------------------------------------------------------------------------------   
			tx_o           : out std_logic 								   			;	      --data out
			tx_busy        : out std_logic                                          ;         --tx busy or not
			tx_done_tick_o : out std_logic							       	        ;		  --data out information tick
			busy_led       : out std_logic                                                    --Busy led under board
			
			);                                                                         
																	       	        
end component;

component uart_rx_module is

	generic ( c_clkfreq           : integer := 20_000_000                          ;		  --Internal clock freq
			  c_baudrate          : integer := 115200                                		  --Communication speed  
			);                    
						          
	port	(
			 ------------------------------------------------------------------------------------------------------------                        
			 --------------------------------------------INPUTS----------------------------------------------------------                        
			 ------------------------------------------------------------------------------------------------------------    
			  clk  		          : in std_logic									;		  --Clock input
			  rx_i 	              : in std_logic									;		  --Rx value input
			  rst                 : in std_logic := '0'                             ;         --State reset button
	         ------------------------------------------------------------------------------------------------------------                                    
			 --------------------------------------------OUTPUTS---------------------------------------------------------                                   
			 ------------------------------------------------------------------------------------------------------------        		                      
     		  data_o	          : out std_logic_vector (7 downto 0)				;		  --Data output
			  rx_done_tick_o 	  : out std_logic									;	      --Data out information tick
			  frame_err           : out std_logic 									;		  --Frame error flag
			  parity_err 		  : out std_logic                                             --Data isn't true			  
			 
			);

end component; 
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------


begin
-------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Component Instantiation--------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
i_uart_tx : uart_tx_module 

	generic map (
					c_clkfreq        => c_clkfreq 									,		  --Clock frequency
					c_baudrate       => c_baudrate									,		  --Communication speed
					c_stopbit        => c_stopbit 											  --UART stop bit number
				)                                                             		
																					
	port map 	(                                                               	                   
					clk 		     => clk 										,		  --Clock signal
					data_i           => data_i    									,		  --Data 
					tx_start_i       => tx_start_i									,   	  --Tx start command
					tx_o             => tx_o									    ,		  --Tx out data
					rst              => rst                                         ,         --Reset command
					tx_done_tick_o   =>	tx_done_tick_o								,		  --Transmitted complated tick
				    tx_busy          => tx_busy                                     ,         --Tx bus busy or not
				    busy_led         => busy_led                                              --Tx bus busy information led
				);                                                              	         
																					
i_uart_rx : uart_rx_module	
	
	generic map ( 	
					c_clkfreq   			 => c_clkfreq                 	        ,         --Clock frequency
					c_baudrate  			 => c_baudrate							          --Communication speed
				)	
	
				
	port	map	(	
					clk 				     => clk 				  		    	,         --Clock signal
					rx_i 	                 => rx_i 	              		        ,         --Rx data input    
					data_o	                 => data_o                         	    ,         --data output
					rx_done_tick_o 	         => rx_done_tick_o 	                    ,         --Received complated tick
					frame_err   			 =>	frame_err						    , 		  --frame error flag
					parity_err               =>	parity_err							,		  --frame error flag
					rst                      => rst                                 
				    
				);                                                                                
                    
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
end Behavioral;