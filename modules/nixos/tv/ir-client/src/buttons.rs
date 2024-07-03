use mouse_keyboard_input::key_codes::*;

#[derive(Debug)]
pub enum KeyCode {
    Key(u16),
    Invalid,
    Unimplemented,
    Suspend,
}

pub fn get_keycode(code: i8) -> KeyCode {
    use KeyCode::*;
    match code {
        68 => Key(KEY_LEFT),
        69 => Key(KEY_RIGHT),
        66 => Key(KEY_UP),
        67 => Key(KEY_DOWN),
        65 => Key(KEY_ENTER),

        86 => Key(KEY_VOLUMEUP),
        87 => Key(KEY_VOLUMEDOWN),

        88 => Key(KEY_EQUAL), // TODO CHUP
        89 => Key(KEY_MINUS), // TODO CHDN

        -105 => Key(KEY_R),     // rewind
        -102 => Key(KEY_SPACE), // PLAYPAUSE
        -103 => Key(KEY_F),     // forward

        -107 => Key(KEY_ESC),
        -106 => Unimplemented, // TODO Menu
        -108 => Unimplemented, // TODO Screen icon?

        97 => Key(KEY_1),
        98 => Key(KEY_2),
        99 => Key(KEY_3),
        100 => Key(KEY_4),
        101 => Key(KEY_5),
        102 => Key(KEY_6),
        103 => Key(KEY_7),
        104 => Key(KEY_8),
        105 => Key(KEY_9),
        96 => Key(KEY_10),

        112 => Key(KEY_BACKSPACE),
        -111 => Unimplemented,

        -109 => Unimplemented, // TODO Guide
        -101 => Unimplemented, // TODO Rec

        70 => Key(KEY_MUTE), // TODO Red
        71 => Key(KEY_F8),   // TODO Yellow
        72 => Unimplemented, // TODO Green
        73 => Unimplemented, // TODO Blue

        115 => Suspend,

        _ => Invalid,
    }
}
