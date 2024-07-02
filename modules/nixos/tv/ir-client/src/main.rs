use crate::buttons::get_keycode;
use crate::serial::SystemPort;
use mouse_keyboard_input::VirtualDevice;
use robust_arduino_serial::read_i8;
use serial;
use serial::SerialPort;
use std::io::ErrorKind;
use std::thread;
use std::time::Duration;

mod buttons;

// Default arduino according to example
const SETTINGS: serial::PortSettings = serial::PortSettings {
    baud_rate: serial::Baud115200,
    char_size: serial::Bits8,
    parity: serial::ParityNone,
    stop_bits: serial::Stop1,
    flow_control: serial::FlowNone,
};

use colored::Colorize;

fn open_port(serial_port: &str) -> SystemPort {
    loop {
        match serial::open(&serial_port) {
            Ok(mut port) => {
                // seems safe idk
                port.configure(&SETTINGS).unwrap();
                port.set_timeout(Duration::from_secs(1)).unwrap();
                return port;
            }
            Err(e) => {
                println!(
                    "Error: {}, unable to open serial port, retrying in 3...",
                    e.to_string().red()
                );
                dbg!(e.to_string());
                match e.to_string().as_str() {
                    "Permission denied" => println!(
                        "{}",
                        "Make sure you have the necessary permissions to access the device".green()
                    ),
                    "No such file or directory" => {
                        println!("{}", "Make sure the device is plugged in".green())
                    }
                    _ => (),
                }
                thread::sleep(Duration::from_secs(3));
            }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let serial_port: &str = match args.len() {
        0 => "/dev/ttyUSB0",
        _ => &args[0],
    };

    let mut device = match VirtualDevice::default() {
        Ok(d) => d,
        Err(e) => {
            println!(
                "Error: {}, unable to create virtual keyboard.",
                e.to_string().red()
            );
            std::process::exit(1);
        }
    };

    loop {
        let mut port = open_port(serial_port);
        println!("Port {:?} opened successfully opened.", serial_port);

        let mut failed_to_fill_count: u8 = 0;

        loop {
            let data = read_i8(&mut port);
            match data {
                Ok(byte) => {
                    use crate::buttons::KeyCode::*;
                    match get_keycode(byte) {
                        Key(c) => match device.click(c) {
                            Err(e) => println!(
                                "Warn: Unable to press keycode {c}, error message: {}.",
                                e.to_string().red()
                            ),
                            _ => (),
                        },
                        Unimplemented => {
                            dbg!("Unimplemented key");
                        }
                        Invalid => println!("Warn: Invalid key received."),
                        Suspend => {
                            match std::fs::write("/sys/power/state", "mem") {
                                Ok(_) => (),
                                Err(e) => println!(
                                    "Error: {}, unable to suspend system.",
                                    e.to_string().red()
                                ),
                            };
                        }
                    }
                }
                Err(e) => {
                    if failed_to_fill_count > 100 {
                        println!("Error: Unable to read serial device for 100 consecutive tries, attempting to reconnect...");
                        break;
                    }
                    if e.kind() == ErrorKind::UnexpectedEof {
                        failed_to_fill_count += 1;
                    }
                }
            }
            thread::sleep(Duration::from_millis(50));
        }
    }
}
