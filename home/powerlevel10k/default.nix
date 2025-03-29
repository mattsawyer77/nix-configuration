{ pkgs, ... }: {
  home.packages = with pkgs; [ zsh-powerlevel10k ];
  programs.zsh.enable = true;
  home.file.".p10k.zsh".text = builtins.readFile ./.p10k.zsh;
}
