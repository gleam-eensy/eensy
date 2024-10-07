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
  //   case gpio.set_pin_pull(21, gpio.Up) {
  //     Error(_) -> io.println("set_pin_pull error")
  //     _ -> Nil
  //   }
  //   case gpio.set_pin_pull(18, gpio.Up) {
  //     Error(_) -> io.println("set_pin_pull error")
  //     _ -> Nil
  //   }
  let address = 60
  process.sleep(10)
  let i2c_display =
    i2c.open([i2c.Sda(21), i2c.Scl(18), i2c.ClockSpeedHz(100_000)])

  process.sleep(10)
  eensy.erlang_display("After Start")
  let init_display_result = init_display(i2c_display, address)
  eensy.erlang_display("init_display")
  eensy.erlang_display(init_display_result)

  eensy.erlang_display("transmit_bytes")
  let _ = transmit_bytes(list.repeat(<<0xFF>>, 4), i2c_display, address)
  let _ = transmit_bytes(list.repeat(<<0xFF>>, 4), i2c_display, address)
  let _ = transmit_bytes(list.repeat(<<0xFF>>, 4), i2c_display, address)
  let _ = transmit_bytes(list.repeat(<<0xFF>>, 4), i2c_display, address)

  process.sleep(10)
  case i2c.close(i2c_display) {
    Ok(_) -> io.println("closed i2c_display")
    Error(_) -> io.println("error closing i2c_display ")
  }
}

fn transmit_bytes(bytes, i2c, address) {
  case i2c.begin_transmission(i2c, address) {
    Ok(_) -> {
      bytes |> list.map(fn(byte) { i2c.write_byte(i2c, byte) })
      let _ = i2c.end_transmission(i2c)
      Nil
    }
    Error(_) -> Nil
  }
}

fn init_display(i2c, address) {
  // Set display OFF		
  transmit_bytes([<<0x80>>, <<0xAE>>], i2c, address)

  // Set Display Clock Divide Ratio / OSC Frequency	
  transmit_bytes([<<0x80>>, <<0xD4>>], i2c, address)

  // Set Multiplex Ratio
  transmit_bytes([<<0x00>>, <<0xA8>>, <<0x3F>>], i2c, address)

  // Display Offset
  transmit_bytes([<<0x00>>, <<0xD3>>, <<0x00>>], i2c, address)

  // Set Display Start Line
  transmit_bytes([<<0x80>>, <<0x40>>], i2c, address)

  // Charge pump on
  transmit_bytes([<<0x00>>, <<0x8D>>, <<0x14>>], i2c, address)

  // Set Segment Re-Map
  transmit_bytes([<<0x80>>, <<0xA1>>], i2c, address)

  // Set Com Output Scan Direction
  transmit_bytes([<<0x80>>, <<0xC8>>], i2c, address)

  // Set COM Hardware Configuration
  transmit_bytes([<<0x80>>, <<0xDA>>], i2c, address)

  // COM Hardware Configuration
  transmit_bytes([<<0x80>>, <<0x12>>], i2c, address)

  // Contrast 256 steps
  transmit_bytes([<<0x00>>, <<0x81>>, <<0x0F>>], i2c, address)

  // Set Pre-Charge Period
  transmit_bytes([<<0x80>>, <<0xD9>>], i2c, address)

  // Set Pre-Charge Period (0x22 External, 0xF1 Internal)
  transmit_bytes([<<0x80>>, <<0xF1>>], i2c, address)

  // Set VCOMH Deselect Level
  transmit_bytes([<<0x80>>, <<0xDB>>], i2c, address)
  transmit_bytes([<<0x80>>, <<0x40>>], i2c, address)

  // Force On disabled
  transmit_bytes([<<0x80>>, <<0xA4>>], i2c, address)

  // Set all pixels OFF
  transmit_bytes([<<0x80>>, <<0xA4>>], i2c, address)

  // Display not inverted
  transmit_bytes([<<0x80>>, <<0xA6>>], i2c, address)

  // Display enabled
  transmit_bytes([<<0x80>>, <<0xAF>>], i2c, address)

  // Column interval [0,127]
  transmit_bytes([<<0x00>>, <<0x21>>, <<0>>, <<127>>], i2c, address)

  // Page interval [0,7]
  transmit_bytes([<<0x00>>, <<0x22>>, <<0>>, <<7>>], i2c, address)

  // Activate horizontal addressing
  transmit_bytes([<<0x80>>, <<0x20>>], i2c, address)

  
}
// fn set_contrast(i2c) {
//   i2c.begin_transmission(i2c, 60)

//   let _ =
//     [oled_control_byte_cmd_stream, oled_cmd_set_contrast, <<255>>]
//     |> list.map(fn(byte) { i2c.write_byte(i2c, byte) })

//   i2c.end_transmission(i2c)
// }


static esp_err_t init_ssd1306_command(i2c_port_t i2c_num)
{
    i2c_cmd_handle_t cmd = i2c_cmd_link_create();
    i2c_master_start(cmd);
        I2C_MASTER_WRITE_BYTE(cmd, (OLED_I2C_ADDRESS << 1) | I2C_MASTER_WRITE, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CONTROL_BYTE_CMD_STREAM, true); //0x00
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_CHARGE_PUMP, true); // 0x8D
        I2C_MASTER_WRITE_BYTE(cmd, 0x14, true);
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_SEGMENT_REMAP, true); // 0xA1 reverse left-right mapping
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_SET_COM_SCAN_MODE, true); // 0xC8 reverse up-bottom mapping
        I2C_MASTER_WRITE_BYTE(cmd, OLED_CMD_DISPLAY_ON, true); // 0xAF
    i2c_master_stop(cmd);
    esp_err_t err = i2c_master_cmd_begin(i2c_num, cmd, MASTER_COMMAND_TIMEOUT_MS);
    i2c_cmd_link_delete(cmd);
    return err;
}

