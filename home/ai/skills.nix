{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) types mapAttrs;
  inherit (builtins) readFile;
  inherit (mlib) mkOpt;

  skills = mapAttrs (_: file: pkgs.writeTextDir "SKILL.md" (readFile file)) config.mHome.ai.skills;
in
{
  options.mHome.ai.skills = mkOpt (types.attrsOf types.path) { } {
    description = "Skill name to SKILL.md file, shared between all agents";
  };

  config = {
    programs.albedo.skills = skills;
    programs.claude-code.skills = skills;
    programs.opencode.skills = skills;
  };
}
