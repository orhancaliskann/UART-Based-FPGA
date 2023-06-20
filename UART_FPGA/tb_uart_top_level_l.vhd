library IEEE;
use IEEE.STD_LOGIC_1164.ALL; 

entity tb_top_level is 

generic ( 
		 
   		  c_clkfreq 				: integer := 100_000_000					;
		  c_baudrate            	: integer :=     1000000					;
		  c_stopbit				    : integer :=  1
		
		);
		
end tb_top_level;

architecture Behavioral of tb_top_level is

component Top_Level is

generic (
			c_clkfreq    : integer := 100_000_000									;	      --Internal clock freq
			c_baudrate   : integer := 115200										;		  --Communication speed
			c_stopbit    : integer := 1 													  --number of stop bit 
		);		
		
port 	(		
			clk          : in  std_logic														   ;		 --clock input
			rx_i         : in  std_logic														   ;		 --UART rx input
			rst          : in  std_logic :='0'                                       			   ;         --State reset
			tx_o         : out std_logic														   ;		 --UART tx output
			led			 : out std_logic                                            			   ;         --Under Board Pulse     
			err_led_p	 : out std_logic                                                           ;														   		 
		    err_led_f    : out std_logic
		);

end component;		
	


signal 	 clk                      :	std_logic										;
signal 	 tx_start_i               :	std_logic   									;
signal 	 rx_i                     :	std_logic       								;
signal 	 tx_o                     :	std_logic   									;
signal 	 tx_done_tick_o           :	std_logic   									;
signal 	 rx_done_tick_o           :	std_logic   									;
signal 	 rx_busy    	          :	std_logic   									;
signal 	 frame_err                :	std_logic   									;
signal 	 parity_err 			  :	std_logic   									;
signal   data_i            		  : std_logic_vector (7 downto 0)					;
signal   data_o                   : std_logic_vector (10 downto 0)                  ;
signal   bit_cntr_1               : integer range 0 to 11                           ;
signal   rst                      : std_logic := '0'                                ;
signal   err_led_p                : std_logic := '0'                                ;
signal   err_led_f                : std_logic := '0'                                ;
signal   led                      : std_logic                                       ;
-------------------------------------------------------------------------------------
constant c_clock_per			  : time := 10 ns									;
constant c_hex_val_1			  : std_logic_vector(0 to 10):= "11" & x"a0" & "0"	;
constant c_hex_val_2              : std_logic_vector(0 to 10):= "11" & x"89" & "0"  ;
constant c_hex_val_3              : std_logic_vector(0 to 10):= "11" & x"66" & "0"  ;
constant c_hex_val_4              : std_logic_vector(0 to 10):= "11" & x"35" & "0"  ;
constant c_hex_val_5              : std_logic_vector(0 to 10):= "11" & x"43" & "0"  ;
constant c_baud 				  : time := 1.0 us							        ;


begin

DUT : Top_Level

generic map(
			c_clkfreq    => c_clkfreq 												, 
			c_baudrate   => c_baudrate												,
			c_stopbit    => c_stopbit
		   )		
		
port map   (		
			clk          => clk														,
			rx_i         => rx_i                                                    ,
			rst          => rst                                                     ,
			tx_o         => tx_o                                                    ,
			led          => led                                                     ,
			err_led_p    => err_led_p                                               ,
			err_led_f    => err_led_f    
		   );

P_CLKGEN : process begin 
	
	clk 	<=	'0'																	;
	wait for c_clock_per/2  														;
	clk     <=  '1' 																;
	wait for c_clock_per/2                                                          ;
end process P_CLKGEN;

P_STIMULI : process begin
rst <= '1';
wait for 3*c_clock_per;
rst <= '0';


wait for 5 * c_baud;

for i in 0 to 10 loop  
	rx_i <= c_hex_val_1(10-i)													;
	wait for c_baud     														;
end loop;

wait for 5 * c_baud;

for i in 0 to 10 loop    
	rx_i <= c_hex_val_2(10-i)													;
	wait for c_baud     														;
end loop;

wait for 5 * c_baud;

for i in 0 to 10 loop      
	rx_i <= c_hex_val_3(10-i)													;
	wait for c_baud     														;
end loop;

wait for 5 * c_baud;

for i in 0 to 10 loop      
	rx_i <= c_hex_val_4(10-i)													;
	wait for c_baud     														;
end loop;

wait for 5 * c_baud;

for i in 0 to 10 loop      
	rx_i <= c_hex_val_5(10-i)													;
	wait for c_baud     														;
end loop;

wait;
end process P_STIMULI;
end Behavioral;