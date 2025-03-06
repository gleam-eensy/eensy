import eensy
import eensy/i2c
import gleam/erlang/process

pub fn start(address address: Int, sda sda: Int, scl scl: Int) {
  let i2c_bus =
    i2c.open([i2c.Sda(sda), i2c.Scl(scl), i2c.ClockSpeedHz(1_000_000)])

  let _ = init_display(i2c_bus, address)

  i2c_bus
}

pub fn transmit_bytes(bytes, i2c i2c: Int, address address: Int) {
  case i2c.begin_transmission(i2c, address) {
    Ok(_) -> {
      case i2c.write_bytes(i2c, bytes) {
        Ok(_) -> Nil
        Error(reason) -> eensy.erlang_display(reason)
      }
      process.sleep(100)

      case i2c.end_transmission(i2c) {
        Ok(_) -> Nil
        Error(reason) -> eensy.erlang_display(reason)
      }
      Nil
    }
    Error(_) -> Nil
  }
}

pub fn init_display(i2c, address) {
  <<
    0x00,
    //display off
    0xAE, 0x20,
    //Set Memory Addressing Mode
    0x20,
    //00,Horizontal Addressing Mode;01,Vertical Addressing Mode;10,Page Addressing Mode (RESET);11,Invalid
    0x00,
    //Set Page Start Address for Page Addressing Mode,0-7
    0xB0,
    //Set COM Output Scan Direction
    0xC8,
    // // Set Com Output Scan Direction  OLED_COMSCANDEC
    // 0xD5,
    //---set low column address
    0x00,
    //---set high column address
    0x10,
    //--set start line address
    0x40,
    //--set contrast control register
    0x81, 0x7F,
    //--set segment re-map 0 to 127
    0xA1,
    //--set normal display
    0xA6,
    //--set multiplex ratio(1 to 64)
    0xA8, 63,
    //-set display offset
    0xD3, 0x00,
    //--set display clock divide ratio/oscillator frequency
    0xD5, 0xF0,
    //--set pre-charge period
    0xD9, 0x22,
    //--set com pins hardware configuration
    0xDA, 0x12,
    //--set vcomh
    0xDB,
    //0x20,0.77xVcc
    0x20,
    //--set DC-DC enable (Charge Pump)
    0x8D, 0x14,
    //0xa4,Output follows RAM content;0xa5,Output ignores RAM content
    0xA4,
    // Row interval 0,127
    0x21, 0, 127,
    // Page interval 0,7
    0x22, 0, 7,
    //--turn on SSD1306 panel
    0xAF,
  >>
  |> transmit_bytes(i2c, address)
}

pub fn reset_position(
  i2c i2c: Int,
  address address: Int,
  matrix matrix: #(Int, Int, Int, Int),
) {
  <<
    0x00,
    // Row interval 123,127
    0x21,
    matrix.0,
    matrix.1,
    // Page interval 7,7
    0x22,
    matrix.2,
    matrix.3,
    //--turn on SSD1306 panel
    0xAF,
  >>
  |> transmit_bytes(i2c, address)
}

pub fn close(i2c i2c: Int) {
  i2c.close(i2c)
}
