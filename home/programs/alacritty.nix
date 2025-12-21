{...}: {
  config = {
    programs.alacritty = {
      settings = {
        cursor = {
          style = {
            shape = "Beam";
            blinking = "Off";
          };
        };

        hints.enabled = [
          {
            command = "xdg-open";
            hyperlinks = true;
            post_processing = true;
            #regex = "(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-<>\"\\s{-}\\^⟨⟩`]+";
          }
        ];

        # hints.enabled.mouse = {
        #   enabled = true;
        #   mods = "None";
        # };

        keyboard.bindings = [
          {
            action = "Paste";
            key = "V";
            mods = "Alt";
          }
          {
            action = "Copy";
            key = "C";
            mods = "Alt";
          }
          {
            action = "IncreaseFontSize";
            key = "Equals";
            mods = "Control";
          }
          {
            action = "DecreaseFontSize";
            key = "Minus";
            mods = "Control";
          }
          {
            action = "ResetFontSize";
            key = "Key0";
            mods = "Control";
          }
        ];
      };
    };
  };
}
