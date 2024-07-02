#include <IRremote.hpp>

IRrecv irrecv(2);
decode_results results;

#define DECODE_NEC

void setup() {
  while (!Serial) {};
  Serial.begin(115200);

  IrReceiver.begin(2, DO_NOT_ENABLE_LED_FEEDBACK);
}

void send(int code) {
  int a = Serial.write(code);
  Serial.flush();
}

#define DELAY 150

void loop() {
  if (IrReceiver.decode()) {

    if (IrReceiver.decodedIRData.protocol == NEC) {
      int data = IrReceiver.decodedIRData.command;
      IrReceiver.resume();
      //IrReceiver.printIRResultShort(&Serial);

      send(data);
      delay(DELAY);
    } else {
      IrReceiver.resume();
    }
  }
}
