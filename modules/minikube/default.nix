{ pkgs
# , listenerURL ? "https://127.0.0.1:6443"
, ...
}:

with pkgs;
{
  environment.systemPackages = [ minikube ];
  # services.k3s = {
  #   enable = true;
  #   package = k3s;
  #   serverAddr = listenerURL;
  #   extraFlags = ''
  #     --write-kubeconfig-mode 644
  #   '';
  # };
}
