{ pkgs, ... }: {
  home.packages = with pkgs; [ zsh-powerlevel10k ];
  home.file.".p10k.zsh".text = builtins.readFile ./.p10k.zsh;
  programs.zsh = {
    enable = true;
    initContent = "source ~/.p10k.zsh";
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];
  };
}
