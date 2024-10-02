-module(eensy_ffi).

% -export([start_with_result/0, set_pin_mode_with_result/2, digital_write_with_result/2]).
-export([
    % GPIO
    start_with_result/0,set_pin_mode_with_result/2, digital_write_with_result/2,
    wait_for_ap_with_result/0,

    % I2C
    i2c_open_with_result/1
]).


% GPIO --------------------------------------------------------------------

start_with_result() ->
    case gpio:start() of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, _} = E -> E
    end.

set_pin_mode_with_result(_Pin, _Direction) ->
    case gpio:set_pin_mode(_Pin, _Direction) of
        ok -> {ok, _Pin};
        error -> {error, nil};
        {error, _} = E -> E
    end.

digital_write_with_result(_Pin, _Level) ->
    case gpio:digital_write(_Pin, _Level) of
        ok -> {ok, _Pin};
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


% I2C ----------------------------------------------------------------------


% i2c_open_with_result(Params) ->
%     erlang:display(Params),
%     case i2c:open(Params) of
%         ok -> {ok, nil};
%         error -> {error, nil};
%         {error, _} = E -> E
%     end.


i2c_open_with_result(Params) ->
    erlang:display(Params),
    I2C = i2c:open(Params),
    ok = i2c:begin_transmission(I2C, 60),
    % ok = i2c:write_bytes(I2C, <<"012345678890123456">>),
    init_ssd1306(I2C),
    loop(1, I2C),
    ok = i2c:end_transmission(I2C),
    I2C.
    % case i2c:open(Params) of
    %     ok -> {ok, nil};
    %     error -> {error, nil};
    %     {error, _} = E -> E
    % end.

% hex_to_bin(Str) -> << << (erlang:list_to_integer([H], 16)):4 >> || H <- Str >>.

init_ssd1306(I2C) ->
    % Based on https://gist.github.com/pulsar256/564fda3b9e8fc6b06b89
    % http://www.adafruit.com/datasheets/UG-2864HSWEG01.pdf Chapter 4.4 Page 15
    ok = i2c:write_byte(I2C, <<"®">>), % "AE" - Set display OFF		
    ok = i2c:write_byte(I2C, <<"Ô">>), % "D4" - Set Display Clock Divide Ratio / OSC Frequency
    ok = i2c:write_byte(I2C, <<128>>), % "80" - Display Clock Divide Ratio / OSC Frequency 
    ok = i2c:write_byte(I2C, <<"¨">> ), % "A8" - Set Multiplex Ratio
    ok = i2c:write_byte(I2C, <<"?">>), % "3F" - Multiplex Ratio for 128x64 (64-1)
    ok = i2c:write_byte(I2C, <<"Ó">>), % "D3" - Set Display Offset
    ok = i2c:write_byte(I2C, <<0>>), % "00" - Display Offset
    ok = i2c:write_byte(I2C, <<"@">>), % "40" - Set Display Start Line
    ok = i2c:write_byte(I2C, <<141>>), % hex_to_bin("8D") - Set Charge Pump
    ok = i2c:write_byte(I2C, <<20>>), % hex_to_bin("14") - Charge Pump (0x10 External, 0x14 Internal DC/DC) 
    ok = i2c:write_byte(I2C, <<"¡">>), % hex_to_bin("A1") - Set Segment Re-Map
    ok = i2c:write_byte(I2C, <<"È">>), % hex_to_bin("C8") - Set Com Output Scan Direction
    ok = i2c:write_byte(I2C, <<"Ú">>), %  hex_to_bin("DA") - Set COM Hardware Configuration
    ok = i2c:write_byte(I2C, <<18>>), % hex_to_bin("12") - COM Hardware Configuration
    ok = i2c:write_byte(I2C, <<129>>), % hex_to_bin("81") - Set Contrast
    ok = i2c:write_byte(I2C, <<"Ï">>), % hex_to_bin("CF") - Contrast
    ok = i2c:write_byte(I2C, <<"Ù">>), % hex_to_bin("D9") - Set Pre-Charge Period
    ok = i2c:write_byte(I2C, <<"ñ">>), % hex_to_bin("F1") Set Pre-Charge Period (0x22 External, 0xF1 Internal)
    ok = i2c:write_byte(I2C, <<"Û">>), % hex_to_bin("DB") Set VCOMH Deselect Level
    ok = i2c:write_byte(I2C, <<"@">>), % hex_to_bin("40") VCOMH Deselect Level
    ok = i2c:write_byte(I2C, <<"¤">>), % hex_to_bin("A4") - Set all pixels OFF
    ok = i2c:write_byte(I2C, <<"¦">>), % hex_to_bin("A6") - Set display not inverted
    ok = i2c:write_byte(I2C, <<"¯">>), % hex_to_bin("AF") - Set display On
    i2c_OLED_clear_display(0, I2C).

loop(Cycle, I2C) ->
    if
        Cycle < 100 ->
            ok = i2c:write_byte(I2C, 101 + Cycle),
            ok = i2c:write_byte(I2C, 110),
            ok = i2c:write_byte(I2C, 111),
            ok = i2c:write_byte(I2C, 80),
            ok = i2c:write_byte(I2C, 100 + Cycle),
            ok = i2c:write_byte(I2C, 120),
            ok = i2c:write_byte(I2C, 112),
            ok = i2c:write_byte(I2C, 0),
            ok = i2c:write_byte(I2C, 121 + Cycle),
            ok = i2c:write_byte(I2C, 120),
            ok = i2c:write_byte(I2C, 90),
            ok = i2c:write_byte(I2C, 120),
            ok = i2c:write_byte(I2C, 111 + Cycle),
            ok = i2c:write_byte(I2C, 151),
            ok = i2c:write_byte(I2C, 111),
            ok = i2c:write_byte(I2C, 160),
            ok = i2c:write_byte(I2C, 100 + Cycle),
            ok = i2c:write_byte(I2C, 170),
            ok = i2c:write_byte(I2C, 101 + Cycle),
            ok = i2c:write_byte(I2C, 151),
            ok = i2c:write_byte(I2C, 181),
            loop(Cycle + 1, I2C);
        true ->
            nil
    end.
    

i2c_OLED_clear_display(Cycle, I2C) -> 
    if
        Cycle < 1024 ->
            ok = i2c:write_byte(I2C, 0),
            i2c_OLED_clear_display(Cycle + 1, I2C);
        true ->
            nil
    end.
    