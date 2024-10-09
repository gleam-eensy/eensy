import eensy
import eensy/gpio
import eensy/i2c
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/result

// Control byte
const oled_control_byte_cmd_single = <<0x80>>

const oled_control_byte_cmd_stream = <<0x00>>

const oled_control_byte_data_stream = <<0x40>>

// Fundamental commands (pg.28)

// follow with 0x7F
const oled_cmd_set_contrast = <<0x81>>

const oled_cmd_display_ram = <<0xA4>>

const oled_cmd_display_allon = <<0xA5>>

const oled_cmd_display_normal = <<0xA6>>

const oled_cmd_display_inverted = <<0xA7>>

const oled_cmd_display_off = <<0xAE>>

const oled_cmd_display_on = <<0xAF>>

// Addressing Command Table (pg.30)
// follow with 0x00 = HORZ mode = Behave like a KS108 graphic LCD
const oled_cmd_set_memory_addr_mode = <<0x20>>

// can be used only in HORZ/VERT mode - follow with 0x00 and 0x7F = COL127
const oled_cmd_set_column_range = <<0x21>>

// can be used only in HORZ/VERT mode - follow with 0x00 and 0x07 = PAGE7
const oled_cmd_set_page_range = <<0x22>>

// Hardware Config (pg.31)
const oled_cmd_set_display_start_line = <<0x40>>

const oled_cmd_set_segment_remap = <<0xA1>>

// follow with 0x3F = 64 MUX
const oled_cmd_set_mux_ratio = <<0xA8>>

const oled_cmd_set_com_scan_mode = <<0xC8>>

// follow with 0x00
const oled_cmd_set_display_offset = <<0xD3>>

// follow with 0x12
const oled_cmd_set_com_pin_map = <<0xDA>>

// NOP
const oled_cmd_nop = <<0xE3>>

// Timing and Driving Scheme (pg.32)
// follow with 0x80
const oled_cmd_set_display_clk_div = <<0xD5>>

const oled_cmd_set_precharge = <<0xD9>>

// follow with 0xF1
// follow with 0x30
const oled_cmd_set_vcomh_deselect = <<0xDB>>

// Charge Pump (pg.62)
// follow with 0x14
const oled_cmd_set_charge_pump = <<0x8D>>

pub fn start() {
  let address = 60

  let i2c_display =
    i2c.open([i2c.Sda(21), i2c.Scl(18), i2c.ClockSpeedHz(400_000)])

  process.sleep(10)
  eensy.erlang_display("After Start")
  let init_display_result = init_display(i2c_display, address)
  eensy.erlang_display("init_display")
  eensy.erlang_display(init_display_result)

  eensy.erlang_display("transmit_bytes")

  let _ =
    [list.repeat(<<0x00>>, 3)]
    |> list.each(fn(_) {
      transmit_bytes(
        [oled_control_byte_data_stream, ..list.repeat(<<0xEE>>, 1024)],
        i2c_display,
        address,
      )
    })

  eensy.erlang_display("done")
  case i2c.close(i2c_display) {
    Ok(_) -> io.println("closed i2c_display")
    Error(_) -> io.println("error closing i2c_display ")
  }
}

fn transmit_bytes(bytes, i2c, address) {
  case i2c.begin_transmission(i2c, address) {
    Ok(_) -> {
      bytes
      |> list.each(fn(byte) {
        case i2c.write_byte(i2c, byte) {
          Ok(_) -> Nil
          Error(reason) -> eensy.erlang_display(reason)
        }
      })
      case i2c.end_transmission(i2c) {
        Ok(_) -> Nil
        Error(reason) -> eensy.erlang_display(reason)
      }
      Nil
    }
    Error(_) -> Nil
  }
}

fn init_display(i2c, address) {
  // 	SSD1306_WRITECOMMAND(0xAE); //display off
  // SSD1306_WRITECOMMAND(0x20); //Set Memory Addressing Mode
  // SSD1306_WRITECOMMAND(0x10); //00,Horizontal Addressing Mode;01,Vertical Addressing Mode;10,Page Addressing Mode (RESET);11,Invalid
  // SSD1306_WRITECOMMAND(0xB0); //Set Page Start Address for Page Addressing Mode,0-7
  // SSD1306_WRITECOMMAND(0xC8); //Set COM Output Scan Direction
  // SSD1306_WRITECOMMAND(0x00); //---set low column address
  // SSD1306_WRITECOMMAND(0x10); //---set high column address
  // SSD1306_WRITECOMMAND(0x40); //--set start line address
  // SSD1306_WRITECOMMAND(0x81); //--set contrast control register
  // SSD1306_WRITECOMMAND(0xFF);
  // SSD1306_WRITECOMMAND(0xA1); //--set segment re-map 0 to 127
  // SSD1306_WRITECOMMAND(0xA6); //--set normal display
  // SSD1306_WRITECOMMAND(0xA8); //--set multiplex ratio(1 to 64)
  // SSD1306_WRITECOMMAND(0x3F); //
  // SSD1306_WRITECOMMAND(0xA4); //0xa4,Output follows RAM content;0xa5,Output ignores RAM content
  // SSD1306_WRITECOMMAND(0xD3); //-set display offset
  // SSD1306_WRITECOMMAND(0x00); //-not offset
  // SSD1306_WRITECOMMAND(0xD5); //--set display clock divide ratio/oscillator frequency
  // SSD1306_WRITECOMMAND(0xF0); //--set divide ratio
  // SSD1306_WRITECOMMAND(0xD9); //--set pre-charge period
  // SSD1306_WRITECOMMAND(0x22); //
  // SSD1306_WRITECOMMAND(0xDA); //--set com pins hardware configuration
  // SSD1306_WRITECOMMAND(0x12);
  // SSD1306_WRITECOMMAND(0xDB); //--set vcomh
  // SSD1306_WRITECOMMAND(0x20); //0x20,0.77xVcc
  // SSD1306_WRITECOMMAND(0x8D); //--set DC-DC enable
  // SSD1306_WRITECOMMAND(0x14); //
  // SSD1306_WRITECOMMAND(0xAF); //--turn on SSD1306 panel
  [
    [oled_control_byte_cmd_stream],
    // // Set DISPLAY Off
    [<<0xAE>>],
    // Set Contrast (256 steps)
    [<<0x81>>],
    [<<0x7F>>],
    // Display not inverted
    [<<0xA6>>],
    // SSD1306_DEACT_SCROLL
    [<<0x2E>>],
    // Activate horizontal addressing / Set Memory Addressing Mode
    [<<0x20>>],
    [<<0x02>>],
    // Set Segment Re-Map
    [<<0xA1>>],
    // Set Multiplex Ratio
    [<<0xA8>>],
    [<<63>>],
    // Set Com Output Scan Direction >> OLED_COMSCANDEC
    [<<0xC8>>],
    // Display Offset
    [<<0xD3>>],
    [<<0x00>>],
    // Set COM Pins hardware configuration
    [<<0xDA>>],
    [<<0x12>>],
    // SSD1306_SET_OSC_FREQ
    [<<0xD5>>],
    [<<0x80>>],
    // OLED_SETPRECHARGE,
    [<<0xD9>>],
    [<<0x22>>],
    // OLED_SETVCOMDESELECT,
    [<<0xDB>>],
    [<<0x20>>],
    // Charge pump on
    [<<0x8D>>],
    [<<0x14>>],
    // Set Display Start Line
    // [oled_control_byte_cmd_single, <<0x40>>],
    [<<0x40>>],
    // Force On disabled (Entire display on, Output ignores RAM content)
    [<<0xA4>>],
    // Display On
    [<<0xAF>>],
    //
  ]
  |> list.flatten
  |> fn(list_bytes) {
    eensy.erlang_display(list_bytes)
    list_bytes
  }
  |> transmit_bytes(i2c, address)
}

fn init_display_v0(i2c, address) {
  [
    //       const char initializeCmds[]={
    //       //////// Fundamental Commands
    //       OLED_DISPLAYOFF,          // 0xAE Screen Off
    //       OLED_SETCONTRAST,         // 0x81 Set contrast control
    //       0x7F,                     // 0-FF ... default half way

    //       OLED_DISPLAYNORMAL,       // 0xA6, //Set normal display 

    //       //////// Scrolling Commands
    //       OLED_DEACTIVATE_SCROLL,   // Deactive scroll

    //       //////// Addressing Commands
    //       OLED_SETMEMORYMODE,       // 0x20, //Set memory address mode
    //       OLED_SETMEMORYMODE_PAGE,  // Page

    //       //////// Hardware Configuration Commands
    //       OLED_SEGREMAPINV,         // 0xA1, //Set segment re-map 
    //       OLED_SETMULTIPLEX,        // 0xA8 Set multiplex ratio
    //       0x3F,                     // Vertical Size - 1
    //       OLED_COMSCANDEC,          // 0xC0 Set COM output scan direction
    //       OLED_SETDISPLAYOFFSET,    // 0xD3 Set Display Offset
    //       0x00,                     //
    //       OLED_SETCOMPINS,          // 0xDA Set COM pins hardware configuration
    //       0x12,                     // Alternate com config & disable com left/right

    //       //////// Timing and Driving Settings
    //       OLED_SETDISPLAYCLOCKDIV,  // 0xD5 Set display oscillator frequency 0-0xF /clock divide ratio 0-0xF
    //       0x80,                     // Default value
    //       OLED_SETPRECHARGE,        // 0xD9 Set pre-changed period
    //       0x22,                     // Default 0x22
    //       OLED_SETVCOMDESELECT,     // 0xDB, //Set VCOMH Deselected level
    //       0x20,                     // Default 

    //       //////// Charge pump regulator
    //       OLED_CHARGEPUMP,          // 0x8D Set charge pump
    //       OLED_CHARGEPUMP_ON,       // 0x14 VCC generated by internal DC/DC circuit

    //       // Turn the screen back on...       
    //       OLED_DISPLAYALLONRESUME,  // 0xA4, //Set entire display on/off
    //       OLED_DISPLAYON,           // 0xAF  //Set display on
    //   };

    // Set DISPLAY Off
    [oled_control_byte_cmd_single, <<0xAE>>],
    // Set Contrast (256 steps)
    [oled_control_byte_cmd_stream, <<0x81>>, <<0x7F>>],
    // Display not inverted
    [oled_control_byte_cmd_single, <<0xA6>>],
    // SSD1306_DEACT_SCROLL
    [oled_control_byte_cmd_single, <<0x2E>>],
    // Activate horizontal addressing / Set Memory Addressing Mode
    [oled_control_byte_cmd_stream, <<0x20>>, <<0x02>>],
    // Set Segment Re-Map
    [oled_control_byte_cmd_single, <<0xA1>>],
    // Set Multiplex Ratio
    [oled_control_byte_cmd_stream, <<0xA8>>, <<63>>],
    // Set Com Output Scan Direction >> OLED_COMSCANDEC
    [oled_control_byte_cmd_single, <<0xC8>>],
    // Display Offset
    [oled_control_byte_cmd_stream, <<0xD3>>, <<0x00>>],
    // Set COM Pins hardware configuration
    [oled_control_byte_cmd_stream, <<0xDA>>, <<0x12>>],
    // SSD1306_SET_OSC_FREQ
    [oled_control_byte_cmd_stream, <<0xD5>>, <<0x80>>],
    // OLED_SETPRECHARGE,
    [oled_control_byte_cmd_stream, <<0xD9>>, <<0x22>>],
    // OLED_SETVCOMDESELECT,
    [oled_control_byte_cmd_stream, <<0xDB>>, <<0x20>>],
    // Charge pump on
    [oled_control_byte_cmd_stream, <<0x8D>>, <<0x14>>],
    // Set Display Start Line
    // [oled_control_byte_cmd_single, <<0x40>>],
    // // Force On disabled (Entire display on, Output ignores RAM content)
    [oled_control_byte_cmd_single, <<0xA4>>],
    // Display On
    [oled_control_byte_cmd_single, <<0xAF>>],
    //

  // // Set Memory Addressing Mode
  // [<<0x00>>, <<0x20>>, <<0x00>>],
  // // COM Hardware Configuration
  // // [<<0x80>>, <<0x02>>],
  // [<<0x80>>, <<0x12>>],
  // // SSD1306_SET_PRECHARGE
  // // [<<0x00>>, <<0xD9>>, <<0xC2>>],
  // [<<0x00>>, <<0xD9>>, <<0xF1>>],
  // // Set VCOMH Deselect Level
  // [<<0x00>>, <<0xDB>>, <<0x30>>],
  // // SSD1306_DEACT_SCROLL
  // [<<0x80>>, <<0x2E>>],

  // // Column interval [0,127]
  // [<<0x00>>, <<0x21>>, <<0>>, <<127>>],
  // // Page interval [0,7]
  // [<<0x00>>, <<0x22>>, <<0>>, <<7>>],
  ]
  |> list.each(fn(cmd) { transmit_bytes(cmd, i2c, address) })
  //

  // // Activate horizontal addressing
  // transmit_bytes([<<0x80>>, <<0x20>>], i2c, address)
  // ========

  // // Set Display Clock Divide Ratio / OSC Frequency	
  // transmit_bytes([<<0x80>>, <<0xD4>>], i2c, address)

  // // Display Offset
  // transmit_bytes([<<0x00>>, <<0xD3>>, <<0x00>>], i2c, address)

  // // Charge pump on
  // transmit_bytes([<<0x00>>, <<0x8D>>, <<0x14>>], i2c, address)

  // // Set Segment Re-Map
  // transmit_bytes([<<0x80>>, <<0xA1>>], i2c, address)

  // // Set Com Output Scan Direction
  // transmit_bytes([<<0x80>>, <<0xC8>>], i2c, address)

  // // Set COM Hardware Configuration
  // transmit_bytes([<<0x80>>, <<0xDA>>], i2c, address)

  // // Contrast 256 steps
  // transmit_bytes([<<0x00>>, <<0x81>>, <<0x0F>>], i2c, address)

  // // Set Pre-Charge Period
  // transmit_bytes([<<0x80>>, <<0xD9>>], i2c, address)

  // // Set Pre-Charge Period (0x22 External, 0xF1 Internal)
  // transmit_bytes([<<0x80>>, <<0xF1>>], i2c, address)

  // // Set VCOMH Deselect Level
  // transmit_bytes([<<0x80>>, <<0xDB>>], i2c, address)
  // transmit_bytes([<<0x80>>, <<0x40>>], i2c, address)

  // // Force On disabled
  // transmit_bytes([<<0x80>>, <<0xA4>>], i2c, address)

  // // Set all pixels OFF
  // transmit_bytes([<<0x80>>, <<0xA4>>], i2c, address)

  // // Display enabled
  // transmit_bytes([<<0x80>>, <<0xAF>>], i2c, address)

  // // Column interval [0,127]
  // transmit_bytes([<<0x00>>, <<0x21>>, <<0>>, <<127>>], i2c, address)

  // // Page interval [0,7]
  // transmit_bytes([<<0x00>>, <<0x22>>, <<0>>, <<7>>], i2c, address)

  // // Activate horizontal addressing
  // transmit_bytes([<<0x80>>, <<0x20>>], i2c, address)
}
// fn set_contrast(i2c) {
//   i2c.begin_transmission(i2c, 60)

//   let _ =
//     [oled_control_byte_cmd_stream, oled_cmd_set_contrast, <<255>>]
//     |> list.map(fn(byte) { i2c.write_byte(i2c, byte) })

//   i2c.end_transmission(i2c)
// }

// static esp_err_t init_ssd1306_command(i2c_port_t i2c_num)
// {
//     i2c_cmd_handle_t cmd = i2c_cmd_link_create();
//     i2c_master_start(cmd);
//         I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
//         I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true); //0x00
//         I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_CHARGE_PUMP, true); // 0x8D
//         I2C_MASTER_WRITE_BYTE(cmd, 0x14, true);
//         I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_SEGMENT_REMAP, true); // 0xA1 reverse left-right mapping
//         I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_COM_SCAN_MODE, true); // 0xC8 reverse up-bottom mapping
//         I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_DISPLAY_ON, true); // 0xAF
//     i2c_master_stop(cmd);
//     esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
//     i2c_cmd_link_delete(cmd);
//     return err;
// }
