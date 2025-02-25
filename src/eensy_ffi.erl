-module(eensy_ffi).

-include("ledc.hrl").

% -export([start_with_result/0, set_pin_mode_with_result/2, digital_write_with_result/2]).
-export([
    % GPIO
    start_with_result/0, set_pin_mode_with_result/2, digital_write_with_result/2,
    wait_for_ap_with_result/0, digital_read_with_result/1,

    % System
    processes_info/0,

    % I2C
    i2c_close_with_result/1, i2c_begin_transmission_with_result/2, 
    i2c_end_transmission_with_result/1, i2c_write_byte_with_result/2,
    i2c_write_bytes_with_result/2, i2c_read_bytes_with_result/3,

    % LEDC
    ledc_test/1

]).


% GPIO --------------------------------------------------------------------

start_with_result() ->
    case gpio:start() of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.


set_pin_mode_with_result(Pin, Direction) ->
    case gpio:set_pin_mode(Pin, Direction) of
        ok -> {ok, Pin};
        error -> {error, nil};
        {error, _} = E -> E
    end.

digital_write_with_result(Pin, Level) ->
    case gpio:digital_write(Pin, Level) of
        ok -> {ok, Pin};
        error -> {error, nil};
        {error, _} = E -> E
    end.

digital_read_with_result(Pin) ->
    Result = gpio:digital_read(Pin),
    case Result of
        high -> {ok, high};
        low -> {ok, low};
        error -> {error, nil};
        {error, _} = E -> E
    end.


% NETWORK ------------------------------------------------------------------


wait_for_ap_with_result() ->
    case network:wait_for_ap() of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.


% System -------------------------------------------------------------------


processes_info() -> 
    Processes = lists:map(
        fun(Pid)-> 
            erlang:display(Pid), 
            ProcessInfo = [erlang:process_info(Pid, stack_size), erlang:process_info(Pid, heap_size), erlang:process_info(Pid, memory)],
            erlang:display(ProcessInfo),
            ProcessInfo
        end,
        [erlang:processes()]
    ),
    erlang:display(Processes).



% I2C ----------------------------------------------------------------------

i2c_close_with_result(I2C) ->
    case i2c:close(I2C) of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.


i2c_begin_transmission_with_result(I2C, Address) -> 
    case i2c:begin_transmission(
        I2C, 
        Address
    ) of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.

i2c_end_transmission_with_result(I2C) -> 
    case i2c:end_transmission(I2C) of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.

i2c_write_byte_with_result(I2C, Byte) -> 
    <<ByteAsInt:8/integer, _Rest/binary>> = Byte,
    case i2c:write_byte(I2C, ByteAsInt) of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.

i2c_write_bytes_with_result(I2C, Bytes) -> 
    erlang:display(erlang:timestamp()),
    Result = i2c_write_bytes_loop(I2C, Bytes),
    erlang:display(erlang:timestamp()),
    Result.
    % case i2c:write_bytes(I2C, Bytes) of
    %     ok -> {ok, nil};
    %     error -> {error, nil};
    %     {error, _} = E -> E
    % end.

i2c_write_bytes_loop(I2C, Bytes) -> 
    case Bytes of
        <<>> -> {ok, nil};
        _ ->
            <<ByteAsInt:8/integer, Rest/binary>> = Bytes,
            case i2c:write_byte(I2C, ByteAsInt) of 
                ok -> i2c_write_bytes_loop(I2C, Rest);
                error -> {error, nil};
                {error, _} = E -> E
            end
    end.


i2c_read_bytes_with_result(I2C, Address, Count) -> 
    case i2c:read_bytes(I2C, Address, Count) of
        {ok, Bytes} -> {ok, Bytes};
        error -> {error, nil};
        {error, _} = E -> E
    end.


% LEDC ----------------------------------------------------------------------


ledc_test(Pin) -> 
    erlang:display(erlang:timestamp()),
    erlang:display('ledc_test'),
    
    %% create a 5khz timer
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    Channel = ?LEDC_CHANNEL_0,
    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]),
    %% bind pin to this timer in a channel
    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, Pin},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]),
    %% set the duty cycle to 0, and fade up to 16000 over 5 seconds
    ledc:set_duty(SpeedMode, Channel, 0),
    ledc:update_duty(SpeedMode, Channel),
    TargetDuty = 4000,
    FadeMs = 1000,
    erlang:display(erlang:timestamp()),
    ok = ledc:fade_func_install(),
    ok = ledc:set_fade_with_time(SpeedMode, Channel, TargetDuty, FadeMs),
    erlang:display(erlang:timestamp()).



% TODO: remove anything not needed below

% % i2c_open_with_result(Params) ->
% %     erlang:display(Params),
% %     case i2c:open(Params) of
% %         ok -> {ok, nil};
% %         error -> {error, nil};
% %         {error, _} = E -> E
% %     end.


    
    % init_ssd1306(I2C),
    % loop(1, I2C),
    % i2c_OLED_fill_display(0, I2C, 255),
    % timer:sleep(1),
    % i2c_OLED_fill_display(0, I2C, 0),
    % timer:sleep(1),
    % i2c_OLED_fill_display(0, I2C, 255),
    % timer:sleep(1),
    % i2c_OLED_fill_display(0, I2C, 0),
    
    % case i2c:open(Params) of
    %     ok -> {ok, nil};
    %     error -> {error, nil};
    %     {error, _} = E -> E
    % end.

% % hex_to_bin(Str) -> << << (erlang:list_to_integer([H], 16)):4 >> || H <- Str >>.

% init_ssd1306(I2C) ->
%     % Based on https://gist.github.com/pulsar256/564fda3b9e8fc6b06b89
%     % http://www.adafruit.com/datasheets/UG-2864HSWEG01.pdf Chapter 4.4 Page 15
%     io.debug(<<0xAE>>)
%     ok = i2c:write_byte(I2C, <<"®">>), % "AE" - Set display OFF		
%     ok = i2c:write_byte(I2C, <<"Ô">>), % "D4" - Set Display Clock Divide Ratio / OSC Frequency
%     ok = i2c:write_byte(I2C, <<128>>), % "80" - Display Clock Divide Ratio / OSC Frequency 
%     ok = i2c:write_byte(I2C, <<"¨">> ), % "A8" - Set Multiplex Ratio
%     ok = i2c:write_byte(I2C, <<"?">>), % "3F" - Multiplex Ratio for 128x64 (64-1)
%     ok = i2c:write_byte(I2C, <<"Ó">>), % "D3" - Set Display Offset
%     ok = i2c:write_byte(I2C, <<0>>), % "00" - Display Offset
%     ok = i2c:write_byte(I2C, <<"@">>), % "40" - Set Display Start Line
%     ok = i2c:write_byte(I2C, <<141>>), % hex_to_bin("8D") - Set Charge Pump
%     ok = i2c:write_byte(I2C, <<20>>), % hex_to_bin("14") - Charge Pump (0x10 External, 0x14 Internal DC/DC) 
%     ok = i2c:write_byte(I2C, <<"¡">>), % hex_to_bin("A1") - Set Segment Re-Map
%     ok = i2c:write_byte(I2C, <<"È">>), % hex_to_bin("C8") - Set Com Output Scan Direction
%     ok = i2c:write_byte(I2C, <<"Ú">>), %  hex_to_bin("DA") - Set COM Hardware Configuration
%     ok = i2c:write_byte(I2C, <<18>>), % hex_to_bin("12") - COM Hardware Configuration
%     ok = i2c:write_byte(I2C, <<129>>), % hex_to_bin("81") - Set Contrast
%     ok = i2c:write_byte(I2C, <<"Ï">>), % hex_to_bin("CF") - Contrast
%     ok = i2c:write_byte(I2C, <<"Ù">>), % hex_to_bin("D9") - Set Pre-Charge Period
%     ok = i2c:write_byte(I2C, <<"ñ">>), % hex_to_bin("F1") Set Pre-Charge Period (0x22 External, 0xF1 Internal)
%     ok = i2c:write_byte(I2C, <<"Û">>), % hex_to_bin("DB") Set VCOMH Deselect Level
%     ok = i2c:write_byte(I2C, <<"@">>), % hex_to_bin("40") VCOMH Deselect Level
%     ok = i2c:write_byte(I2C, <<"¤">>), % hex_to_bin("A4") - Set all pixels OFF
%     ok = i2c:write_byte(I2C, <<"¦">>), % hex_to_bin("A6") - Set display not inverted
%     ok = i2c:write_byte(I2C, <<"¯">>). % hex_to_bin("AF") - Set display On
    

    

% i2c_OLED_fill_display(Cycle, I2C, Byte) ->
%     if
%         Cycle < 1024 ->
%             ok = i2c:write_byte(I2C, Byte),
%             i2c_OLED_fill_display(Cycle + 1, I2C, Byte);
%         true ->
%             nil
%     end.
    

% i2c_OLED_clear_display(I2C, Cycle) -> 
%     if
%         Cycle < 1024 ->
%             ok = i2c:write_byte(I2C, 0),
%             erlang:display("i2c_OLED_clear_display > write_byte"),
%             erlang:display(ok),
%             i2c_OLED_clear_display( I2C, Cycle + 1);
%         true ->
%             nil
%     end.
    