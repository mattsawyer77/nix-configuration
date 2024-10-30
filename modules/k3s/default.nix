{ pkgs
, listenerURL ? "https://127.0.0.1:6443"
, ...
}:

with pkgs;
{
  environment.systemPackages = [ k3s ];
  services.k3s = {
    enable = true;
    package = k3s;
    serverAddr = listenerURL;
    extraFlags = ''
      --write-kubeconfig-mode 644
    '';
  };
}
