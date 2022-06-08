{ config, lib, pkgs, ... }:

{
  environment.shellAliases = {
    ssh = "TERM=xterm-256color ssh";
    socks4proxy = "ssh -D 8888 -f -C -q -N";
    randomizeMacAddress =
      "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
    k = "kubectl";
    l = "exa -alF";
    ts = "tmux new-session -n main -s";
    ta = "tmux attach -t";
    tl = "tmux list-sessions";
    em = "em.zsh";
    doom = "~/.emacs.d/bin/doom";
  };
  programs.zsh.promptInit = ''
    eval $(starship init zsh)
  '';
  programs.zsh.loginShellInit = ''
        launchctl-restart() {
          if [[ -v 1 ]]; then
            pattern="$1"
            services=($(launchctl list | pcregrep "$pattern" | awk '{print $3}'))
            for service in $services; do
              plist=($(find /Library/Launch* ~/Library/LaunchAgents -name "''${service}.plist" | head -1 || :))
              echo "stopping service $service..."
              launchctl unload "$plist" \
                && echo "service $service stopped, restarting..." \
                && launchctl load "$plist" \
                && echo "service $service restarted successfully."
            done
          else
            echo >&2 "must specify a pattern for a service to restart"
          fi
        }

        find-proto-import-path() {
          local dirs=""
          local target_dir="$1"
          (test -d "$target_dir" || echo >&2 "$target_dir is not a valid dir") \
            && dirs=$(cd "$target_dir" \
              && typeset -A import_paths \
              && for import in $(grep -h import *.proto | cut -d'"' -f2); do \
                if [[ -z "''${import_paths[$import]}" ]]; then
                  paths=$(find $(realpath --relative-to . $(git rev-parse --show-toplevel)) -wholename "*$import" \
                    | sd "/$import" "" \
                    | sd '^(.+)$' '    \"$1\"') \
                  import_paths+=( ["$import"]="$paths" )
                fi
                echo "''${import_paths[$import]}"
              done \
            | sort -u)
          echo "$dirs"
        }

        generate-protoc-import-dir-locals() {
          if [[ ! -v 1 ]]; then root_dir=.; else root_dir="$1"; fi
          echo "generating .dir-locals.el for each directory recursively from ''${root_dir}..."
          repo_proto_dirs=($(fd '\.proto$' "$root_dir" -x dirname | sort -u))
          for repo_proto_dir in $repo_proto_dirs; do
            import_dirs=$(find-proto-import-path "$repo_proto_dir")
            if [[ -n "$import_dirs" ]]; then
              echo " - generating ''${repo_proto_dir}/.dir-locals.el"
              cat >"''${repo_proto_dir}/.dir-locals.el" <<EOF
        ((protobuf-mode .
          ((flycheck-protoc-import-path .
            (
        $import_dirs
            )))))
    EOF
            else
              echo " - no import dirs found for $repo_proto_dir"
            fi
          done
        }

        git-cleanup-branches() {
          # prune local tracking branchces that no longer exist on the origin
          git remote prune origin
          # delete local branches that have been merged to main/master
          git branch --merged | pcregrep -v '\*|master|main' | cut -c3- | xargs git branch -d
          # prompt the user for any remaining branches to be deleted
          local branches=($(git branch | pcregrep -v '\*|master|main' | cut -c3-))
          local answer=""
          for branch in $branches; do
            while [[ "$answer" != "n" ]] && [[ "$answer" != "y" ]]; do
              echo -n "delete branch ''${branch} [y/n/d(iff)/q(uit)]? "
              read -r answer
              if echo "$answer" | pcregrep '^d' >/dev/null; then
                git log -p .."$branch"
              elif echo "$answer" | pcregrep '^q' >/dev/null; then
                return
              elif [[ "$answer" == "y" ]]; then
                git branch -D "$branch"
              fi
            done
          done
        }

        # workaround for https://github.com/containers/skopeo/issues/1534
        skopeo-acr-login() {
          az acr login -n volterra --expose-token \
            | jq -r '.accessToken' \
            | skopeo login volterra.azurecr.io \
              --username 00000000-0000-0000-0000-000000000000 \
              --password-stdin
        }

        skopeo-inspect() {
          local url
          if echo "$url" | grep '^docker://' >/dev/null; then
            url="$1"
          else
            url="docker://$1"
          fi
          skopeo inspect "$url" \
            | jq -r '.Labels."commit-sha"'
        }

        skopeo-inspect-digest() {
          local url
          if echo "$url" | grep '^docker://' >/dev/null; then
            url="$1"
          else
            url="docker://$1"
          fi
          skopeo inspect "$url" \
            | jq -r '.Digest'
        }

        skopeo-inspect-commit() {
          commit=$(skopeo-inspect "$1")
          if [[ "$?" -eq 0 ]] && [[ -n "$commit" ]]; then
            git fetch
            git log --all -n 1 "$commit"
          fi
        }

        volterra-ss-commit() {
          local ss="$1"
          image=$(kubectl -n ves-system get statefulset "$ss" -o json \
            | jq -r '.spec.template.spec.containers[]|{image,name}|select(.name=="'"$ss"'")|.image')
          if [[ -z "$image" ]]; then
            echo >&2 "no image found for statefulset $ss"
          else
            skopeo inspect docker://"$image" \
              | jq -r '.Labels."commit-sha"'
          fi
        }

        volterra-ds-commit() {
          local ss="$1"
          image=$(kubectl -n ves-system get daemonset "$ds" -o json \
            | jq -r '.spec.template.spec.containers[]|{image,name}|select(.name=="'"$ds"'")|.image')
          if [[ -z "$image" ]]; then
            echo >&2 "no image found for daemonset $ds"
          else
            skopeo inspect docker://"$image" \
              | jq -r '.Labels."commit-sha"'
          fi
        }

        volterra-envs() {
          local name="$1"
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: name required!\nusage: volterra-envs [name]"
            echo >&2 "example: volterra-envs akar"
          else
            envs=$(kubectl -n ves-system get pod -l "app=$name" -o json \
              | jq -rc '.items|first|.spec.containers[]|select(.name=="'"$name"'")|.env[]|.name+": "+((.valueFrom|tostring)//.value)')
            if [[ -z "$envs" ]]; then
              echo >&2 "no environment variables found for a pod with labels app.kubernetes.io/instance=$name"
            else
              echo "$envs"
            fi
          fi
        }

        volterra-configmap() {
          local name="$1"
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: name required!\nusage: volterra-configmap [name]"
            echo >&2 "example: volterra-configmap akar"
          else
            envs=$(kubectl -n ves-system get configmap -l "app.kubernetes.io/instance=$name" -o json \
              | kv get configmap -l "app.kubernetes.io/instance=$name" -o json \
              | jq -r '.items|first|.data|."config.yml"//."'"$name"'.yml"')
            if [[ -z "$envs" ]]; then
              echo >&2 "no configmap found for a pod with labels $app.kubernetes.io/instance=$name"
            else
              echo "$envs"
            fi
          fi
        }

        volterra-commit() {
          local resource_type="$1"
          local name="$2"
          if [[ ! -v 1 ]] || [[ ! -v 2 ]]; then
            echo >&2 "ERROR: resource type and name are required!\nusage: volterra-commit <k8s-resource-type> <k8s-resource-name> [container name (defaults to resource name)]"
          else
            local container="''${3:-$name}"
            image=$(kubectl -n ves-system get "$resource_type" "$name" -o json \
              | jq -r '.spec.template.spec.containers[]|{image,name}|select(.name=="'"$container"'")|.image')
            if [[ -z "$image" ]]; then
              echo >&2 "no image found for $resource_type $name"
            else
              echo "image: $image"
              if ! command -v skopeo >/dev/null || echo "$image" | grep azurecr.io >/dev/null; then
                # skopeo doesn't work with docker for mac apparently
              docker pull "$image" --quiet \
                && commit_sha=$(docker inspect "$image" \
                | jq -r '.[].Config.Labels."commit-sha"') \
                && repo=$(echo "$image" | perl -pe 's@.*/ves\.io/(\w+).*@\1@')
              else
                commit_sha=$(skopeo inspect docker://"$image" \
                  | jq -r '.Labels."commit-sha"') \
                && repo=$(echo "$image" | perl -pe 's@.*volterraio/(\w+).*@\1@')
              fi
              if [[ -n "$commit_sha" ]] && [[ -n "$repo" ]]; then
                repo_dir=$(realpath ~/workspaces/volterra/ves.io/"$repo")
                if [[ -d "$repo_dir" ]]; then
                  git -C "$repo_dir" log -n 1 origin "$commit_sha" \
                    || (git -C "$repo_dir" fetch --quiet origin && git -C "$repo_dir" log -n 1 origin "$commit_sha")
                else
                  echo >&2 -n "repo dir $repo_dir does not exist"
                fi
              else
                echo >&2 "ERROR: unable to determine commit sha or repo for $image"
              fi
            fi
          fi
        }

        gcr-info() {
          if [[ -v 1 ]]; then
            local repo="$1"
            local branch=''${2:-master}
            gcloud container images list-tags "gcr.io/volterraio/$repo" \
              --format=json \
              --sort-by=timestamp \
              | jq -r '.[]|last(select(.tags|index("branch-'"$branch"'")))'
          else
            echo >&2 "ERROR: must specify a repo"
            false
          fi
        }

        gcr-digest() {
          if [[ -v 1 ]]; then
            local repo="$1"
            local branch=''${2:-master}
            gcloud container images list-tags "gcr.io/volterraio/$repo" \
              --format=json \
              --sort-by=timestamp \
              | jq -r '.[]|last(select(.tags|index("branch-'"$branch"'")))|.digest'
          else
            echo >&2 "ERROR: must specify a repo"
            false
          fi
        }

        volterra-vpn() {
          if ! command -v openfortivpn >/dev/null; then
            echo >&2 "openfortivpn not found."
            return 1
          fi
          local config_file=~/.config/openfortivpn/config
          if [[ -f "$config_file" ]]; then
            local servers=$(pcregrep -o 'vpnssl-.*acorus.net' "$config_file" 2>/dev/null)
            if [[ $? -eq 0 ]] && [[ -n "$servers" ]]; then
              server=$(echo "$servers" \
                | sort -u \
                | fzf)
              if [[ -n "$server" ]]; then
                sudo -E openfortivpn "$server" -c "$config_file"; notify
              else
                echo >&2 "no server selected."
                return 1
              fi
            else
              echo >&2 "no servers found."
              return 1
            fi
          else
            echo >&2 "config file $config_file does not exist"
            return 1
          fi

          echo "clearing DNS cache, which requires sudo:"
          dscacheutil -flushcache && sudo killall -HUP mDNSResponder
        }

        docker-shell () {
          local image="''${1:-$(pcregrep -o 'volterra.*go-builder:v?[\d\.]+' .gitlab-ci.yml)}"
          if [[ -z "$image" ]]; then
            echo >&2 "ERROR: no image specified, and go-builder image could not be derived from .gitlab-ci.yml"
          else
            local cmd="''${2:-bash}"
            local temp_passwd_file="$(mktemp)"
            local project_root="$(git rev-parse --show-toplevel)"
            local go_src_dir=$(echo $project_root | sed "s^.*src/\(.*\)^src/\1^")
            local go_cache_dir="''${project_root}/.cache-docker/go"
            echo "running container shell with $image"
            echo "''${USER}:x:''${UID}:''${GID}:Volterra User:''${HOME}:/bin/bash" > "$temp_passwd_file" \
              && mkdir -p "''${project_root}/.cache.go" \
              && docker run --rm -it --user ''${UID}:''${GID} --userns=host \
                --env GOCACHE=''${go_cache_dir} \
                --env GOPATH=''${GOPATH} \
                --env HOME=''${HOME} \
                --env TERM=xterm-256color \
                --env PS1="''${image}:\w> " \
                --env DOCKER_IMAGE="$image" \
                --net host \
                -v "$temp_passwd_file":/etc/passwd:ro,Z \
                -v ''${PWD}:/go/''${GOPATH}/src:Z \
                -v ''${HOME}/.gitconfig:''${HOME}/.gitconfig:ro \
                -v ''${HOME}/.ssh:''${HOME}/.ssh:ro \
                -v ''${GOPATH}:''${GOPATH} \
                -v ''${project_root}:''${project_root} \
                -v "''${HOME}/.bashrc":"''${HOME}/.bashrc" \
                -w ''${project_root} \
                ''${image} ''${cmd}
            test -f "$temp_passwd_file" && rm -f "$temp_passwd_file"
          fi
        }

        # use after some long-running process to notify you while your
        # brain's executive function is hyperfocusing on something else
        notify() {
          exit_code=$?
          current_cl=''${history[$HISTCMD]}
          last_cmd=$(echo "$current_cl" | sed 's/;[^;]*$//' | sed 's/"//g')
          title="with title \"$last_cmd\""
          if [[ -v 1 ]]; then
            title="with title \"$1\""
          fi
          if [[ $exit_code -eq 0 ]]; then
            osascript -e "display notification \"command succeeded\" $title sound name \"Funk\""
          else
            osascript -e "display notification \"command failed\" $title sound name \"Sosumi\""
          fi
          return $exit_code
        }

        streakctl() {
          GRPC_TLS_PORT=''${STREAK_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap streak-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
          # echo "streak GRPC TLS port: $GRPC_TLS_PORT" >&2
          SERVER_CN=''${STREAK_SERVER_CN:-$(kubectl -n ves-system get statefulset streak -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
          POD_NAME=''${STREAK_POD:-"streak-0"}
          # echo "streak CN: $SERVER_CN" >&2
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            kubectl -n ves-system -c streak exec -it "$POD_NAME" -c streak -- \
              streakctl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
              $@
          else
            echo "could not determine streak's GRPC TLS port"
          fi
        }

        mauricectl() {
          GRPC_TLS_PORT=''${MAURICE_GRPC_TLS_PORT:-6002}
          echo "maurice GRPC TLS port: $GRPC_TLS_PORT" >&2
          SERVER_CN=''${MAURICE_SERVER_CN:-maurice.ves-system.svc.cluster.local}
          echo "maurice CN: $SERVER_CN" >&2
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            maurice_pod=$(kubectl -n ves-system get pods | grep maurice | awk '{print $1}')
            if [[ -n "$maurice_pod" ]]; then
              kubectl -n ves-system -c maurice exec -it "$maurice_pod" -c maurice -- \
                mauricectl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
                $@
            else
              echo "could not find a running maurice pod" >&2
            fi
          else
            echo "could not determine maurice's GRPC TLS port"
          fi
        }

        vulpixctl() {
          GRPC_TLS_PORT=''${VULPIX_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap vulpix-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
          SERVER_CN=''${VULPIX_SERVER_CN:-$(kubectl -n ves-system get deployment vulpix -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            kubectl -n ves-system -c vulpix exec -it deploy/vulpix -c vulpix -- \
              vulpixctl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
              $@ \
              | pcregrep -v 'WARNING|golang/protobuf|which has long been excluded'
          else
            echo "could not determine vulpix's GRPC TLS port"
          fi
        }

        griffinctl() {
          GRPC_TLS_PORT=''${GRIFFIN_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap griffin-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
          echo "griffin GRPC TLS port: $GRPC_TLS_PORT" >&2
          SERVER_CN=''${GRIFFIN_SERVER_CN:-$(kubectl -n ves-system get deployment griffin -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
          echo "griffin CN: $SERVER_CN" >&2
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            griffin_pod=$(kubectl -n ves-system get pods | grep griffin | awk '{print $1}')
            if [[ -n "$griffin_pod" ]]; then
              kubectl -n ves-system -c griffin exec -it "$griffin_pod" -c griffin -- \
                griffinctl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
                $@
            else
              echo "could not find a running griffin pod" >&2
            fi
          else
            echo "could not determine griffin's GRPC TLS port"
          fi
        }

        akarctl() {
          GRPC_TLS_PORT=''${AKAR_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap akar-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
          SERVER_CN=''${AKAR_SERVER_CN:-$(kubectl -n ves-system get deployment akar -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            akar_pod=$(kubectl -n ves-system get pods | grep akar | grep -v readonly | awk '{print $1}')
            if [[ -n "$akar_pod" ]]; then
              kubectl -n ves-system -c akar exec -it "$akar_pod" -c akar -- \
                akarctl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
                $@
            else
              echo "could not find a running akar pod" >&2
            fi
          else
            echo "could not determine akar's GRPC TLS port"
          fi
        }

        rakarctl() {
          rakar_pod=$(kubectl -n ves-system get pods -lapp=rakar --no-headers | head -1 | awk '{print $1}')
          if [[ -z "$rakar_pod" ]]; then
            echo >&2 "could not find rakar pod"
            return 1
          fi
          service_name=$(kubectl -n ves-system get deploy rakar -o yaml | grep -A1 'name: serviceNames' | grep value | awk '{print $2}' | cut -d',' -f1)
          if [[ -z "$service_name" ]]; then
            echo >&2 "could not find rakar common name for cert"
            return 1
          fi
          kubectl -n ves-system exec -it "$rakar_pod" -c rakar -- \
            rakarctl --server-cn "$service_name" -u "localhost:9501" $@
        }

        nioctl() {
          local usage="usage:\nnioctl deployment-name|statefulset-name kubectl-args"
          local service_name="$1"
          if [[ ! -v service_name ]]; then
            echo >&2 "k8s name (pod prefix) is required"
            echo >&2 "$usage"
            return 1
          fi
          shift
          local service_json=$(kubectl -n ves-system get statefulset,daemonset,deployment "$service_name" -o json 2>/dev/null)
          local service=$(printf '%s\n' "$service_json" | jq -r '.items|first' 2>/dev/null)
          if [[ -z "$service" ]]; then
            echo >&2 "could not find a statefulset, daemonset, or deployment named $service_name"
            return 1
          fi
          local kind=$(printf '%s\n' "$service" | jq -r '.kind')
          if [[ -z "$kind" ]]; then
            echo >&2 "could not determine kind of service $service_name"
            return 1
          fi
          GRPC_TLS_PORT=''${NIO_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap "''${service_name}-config" -o json \
            | jq -r '.data."nio.yml"' \
            | yq e '.GrpcTLSPort' -)}
          # echo "nio GRPC TLS port: $GRPC_TLS_PORT" >&2
          SERVER_CN=''${NIO_SERVER_CN:-$(printf '%s\n' "$service" \
            | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames|split(",")|last')}
          # echo "nio CN: $SERVER_CN" >&2
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            nio_pod=$(kubectl -n ves-system get pods --no-headers -l name="''${service_name}" | awk '{print $1}' | head -1)
            if [[ -n "$nio_pod" ]]; then
              kubectl -n ves-system exec -it "$nio_pod" -c nio -- \
                nioctl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
                $@
            else
              echo "could not find a running nio pod for $kind $service_name" >&2
            fi
          else
            echo "could not determine nio's GRPC TLS port"
          fi
        }

        vegactl() {
          local usage="usage:\nvegactl kubectl-args"
          GRPC_TLS_PORT=''${VEGA_GRPC_TLS_PORT:-$(kubectl -n ves-system describe cm ver-config | grep -E '^GrpcTLSPort' | cut -d' ' -f2)}
          SERVER_CN=''${VEGA_SERVER_CN:-$(kubectl -n ves-system describe cm ver-config | grep VegaCommonName | cut -d' ' -f2)}
          # echo "vega CN: $SERVER_CN" >&2
          if [[ -n "$GRPC_TLS_PORT" ]]; then
            vega_pod=$(kubectl -n ves-system get pods --no-headers -l "app=ver,cfg!=ver" | awk '{print $1}' | head -1)
            if [[ -n "$vega_pod" ]]; then
              kubectl -n ves-system exec -it "$vega_pod" -c vega -- \
                vegactl -u "localhost:''${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
                $@
            else
              echo "could not find a running vega pod" >&2
            fi
          else
            echo "could not determine vega's GRPC TLS port"
          fi
        }

        profile-service() {
          usage="usage:\nprofile_service service_name [environment] [pprof_type] [sample_time]"
          if [ ! -v 1 ]; then
            echo >&2 "must specify service to profile"
            echo >&2 "$usage"
            set +x
            return 1
          fi
          local service="$1"
          local environment="''${2:-demo1}"
          local pprof_type="''${3:-profile}"
          local sample_time="''${4:-5}"
          local api_gw_hostname
          local compass_hostname
          case $environment in
            demo1)
              compass_hostname="compass-lma.demo1.volterra.us"
              api_gw_hostname="''${5:-gc01.int.ves.io}"
              ;;
            crt)
              compass_hostname="compass-lma.crt.volterra.us"
              api_gw_hostname="''${5:-gc01.int.ves.io}"
              ;;
            staging)
              compass_hostname="compass-lma.staging.volterra.us"
              api_gw_hostname="''${5:-gc1-iad-01.int.volterra.us}"
              ;;
            *)
              echo >&2 "unknown environment $environment"
              return 1
              ;;
          esac
          output_filename="''${service}.''${environment}.''${pprof_type}.$(date --iso-8601=seconds).pprof"
          url="https://''${compass_hostname}/introspection/''${api_gw_hostname}/''${service}/debug/pprof/''${pprof_type}?seconds=''${sample_time}"
          echo "starting profile of $service on $environment (url: $url)..."
          curl \
            --insecure \
            --fail-with-body \
            --no-progress-meter \
            --cert-type P12 \
            --cert "$HOME/.ves-internal/''${environment}/usercerts.p12:volterra" \
            -o "$output_filename" \
            "$url" \
            && echo "profile saved at $output_filename"
        }

        # kv set image statefulset/streak streak=gcr.io/volterraio/streak@sha256:37be0ca9476b754ed144da6678a414748825e733dfd0345fa3fc1924a559d42a

        set-image() {
          local usage='usage: set-image resource-type resource-name image-sha'
          local resource_type="$1"
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: resource-type is required"
            echo >&2 "$usage"
            return 1
          fi
          local resource_name="$2"
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: resource-name is required"
            echo >&2 "$usage"
            return 1
          fi
          local container_name="$resource_name"
          local image_sha="$3"
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: image-sha is required"
            echo >&2 "$usage"
            return 1
          fi
          if echo "$image_sha" | grep -E '^sha256:[0-9a-f]+$'; then
            image_sha="$(echo $image_sha | cut -d':' -f2)"
          fi
          if echo "$image_sha" | grep -E '^[0-9a-f]+$'; then
            echo >&2 "ERROR: invalid SHA for image: $image_sha"
            return 1
          fi
          echo "updating image for ''${resource_type}/''${resource_name} to sha256:''${image_sha}..."
        }

        # get kubernetes load balancers
        klb() {
          declare -a jq_args
          jq_args=(
            '["name", "clusterIP", "loadBalancerIPs", "ports", "selector"]'
            ', (.items[]|select(.spec.type=="LoadBalancer")|['
            '.metadata.name'
            ', .spec.clusterIP'
            ', (.status.loadBalancer.ingress|map(.ip)|join(", "))'
            ', (.spec.ports|map(.protocol+" "+(.port|tostring)+":"+(.targetPort|tostring)+" ("+.name+")")|join(", "))'
            ', (.spec.selector|to_entries|map(.key+"="+.value)|join("∩"))'
            '])|@tsv'
          )
          kubectl get svc $@ -o json | jq -r "$jq_args" | column -s $'\t' -t
        }

        # get kubernetes services
        ksvc() {
          declare -a jq_args
          jq_args=(
            '["name", "clusterIP", "loadBalancerIPs", "ports", "selector"]'
            ', (.items[]|['
            '.metadata.name'
            ', .spec.clusterIP'
            ', (.status.loadBalancer.ingress//[]|map(.ip)|join(", "))'
            ', (.spec.ports|map(.protocol+" "+(.port|tostring)+":"+(.targetPort|tostring)+" ("+.name+")")|join(", "))'
            ', (.spec.selector|to_entries|map(.key+"="+.value)|join("∩"))'
            '])|@tsv'
          )
          kubectl get svc $@ -o json | jq -r "$jq_args" | column -s $'\t' -t
        }

        kcontainers() {
          kubectl get statefulset,deployment,daemonset -o json $@ | jq -r '.items[]|.kind+": "+.metadata.name+": "+([.spec.template.spec.containers[]|.name]|sort|join(", "))'| column -s':' -t
        }

        introspect() {
          host=$(echo "$@" | pcregrep -o 'compass-lma\.(\w+)\.volterra\.(us|io)')
          environment=$(echo "$host" | cut -d'.' -f2)
          if [[ "$environment" == "ves" ]]; then
            environment="prod"
          fi
          headers="-H 'pragma: no-cache' -H 'accept-encoding: gzip, deflate, br' -H 'accept-language: en-US' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_16_0) AppleWebKit/537.36 (KHTML, like Gecko) sia/1.23.13 Chrome/69.0.3497.128 Electron/4.1.1 Safari/537.36' -H 'access-control-allow-methods: *' -H 'content-type: application/json' -H 'access-control-allow-origin: *' -H 'accept: application/json' -H 'cache-control: no-cache' -H 'authority: ''${host}'"
          user_cert="''${HOME}/.ves-internal/''${environment}/usercerts.p12"
          if [[ -f "$user_cert" ]]; then
            curl --insecure --fail --no-progress-meter --cert-type P12 --cert "''${user_cert}:volterra" $headers $@
          else
            echo >&2 "unknown environment $environment"
            return 1
          fi
        }

        # change docker's kernel's disk driver to write-through mode
        # (fast but less resilient to power loss)
        enable-docker-write-through() {
          docker run -it --rm --privileged --pid=host alpine:edge nsenter -t 1 -m -u -n -i bash -c 'echo "write through" > /sys/class/block/vda/queue/write_cache'
        }

        show-docker-cache-mode() {
          docker run -it --rm --privileged --pid=host alpine:edge nsenter -t 1 -m -u -n -i bash -c 'cat /sys/class/block/vda/queue/write_cache'
        }

        # change docker's kernel's disk driver to write-through mode
        # (slow but more resilient to power loss)
        disable-docker-write-through() {
          docker run -it --rm --privileged --pid=host alpine:edge nsenter -t 1 -m -u -n -i bash -c 'echo "write back" > /sys/class/block/vda/queue/write_cache'
        }

        matrix-renew-certs() {
          matrix certInfo 2>&1 \
            | pcregrep -B4 'status:.*expired' \
            | pcregrep -B2 '^\s*User Certificate' \
            | pcregrep -o '^\w+' \
            | xargs -I% sh -c "echo renewing cert for %...; matrix get-user-cert -e %"
        }

        gc-login() {
          k cluster-info 2>/dev/null \
            || (gcloud auth login --project=devtest-293809 \
            && gcloud container clusters get-credentials gc01-int-ves-io --region us-east4)
        }

        site-terraform-output() {
          if [[ ! -v 1 ]]; then
            echo >&2 "ERROR: site name required"
            echo >&2 "usage example: site-terraform-output ves-io-aws-vpc-site-my-site"
            return 1
          fi
          site_name="$1"
          response=$(vulpixctl introspection list ves.io.vulpix.terraform_parameters.Object \
            --page-limit 5000 \
            --name-filter "$site_name" \
            --store-only)
          if [[ -n "$response" ]]; then
            tf_params_id=$(printf '%s\n' "$response" | yq e '.getResponses|keys|.[0]' - 2>/dev/null)
          else
            echo >&2 "ERROR: could not find terraform params ID for site named '$site_name'"
            return 1
          fi
          if [[ -n "$tf_params_id" ]]; then
            vulpixctl introspection get ves.io.schema.views.terraform_parameters.StatusObject "$tf_params_id" \
              | yq e '.object.applyStatus.tfOutput' -
          else
            echo >&2 "ERROR: could not get terraform output for site named '$site_name'"
            return 1
          fi
        }

        streak-get-status-objects() {
          if [[ $# -lt 2 ]]; then
            echo >&2 "ERROR: status object type and config object UID(s) required"
            echo >&2 "usage example: streak-get-status-objects ves.io.schema.site.StatusObject 4cbcf514-a024-4e95-a7bf-f9e9ffb0f4fa ce7d079e-5352-4571-a564-c2ccc1aa6fe0"
            return 1
          fi
          status_object_type="$1"
          shift
          config_object_uids=()
          while [[ -v 1 ]]; do
            config_object_uids+=(\"$1\")
            shift
          done
          config_objects_str="[$(print -R ''${(j|,|)config_object_uids})]"
          streakctl customAPI ves.io.streak.CustomAPI ListStatusObjects --json-data '{ "status_object_type": "'"$status_object_type"'", "config_object_uids": '"$config_objects_str"' }' | yq e '.'
        }

        alacritty-toggle-theme() {
          local dark_theme="Ocean.dark"
          local light_theme="Ocean.light"
          if ! command -v alacritty-themes >/dev/null; then
            echo -n 'alacritty-themes is not installed. install it now? [y/n]? '
            read -r answer
            if [[ "$answer" == "y" ]]; then
              sudo npm install -g alacritty-themes && alacritty-toggle-theme $@
            fi
          fi
          if [[ $(alacritty-themes --current) == "$dark_theme" ]]; then
            alacritty-themes "$light_theme"
          else
            alacritty-themes "$dark_theme"
          fi
        }
  '';
  # TODO: investigate syntax highlighting and autosuggestions, which aren't loading right now
  programs.zsh.interactiveShellInit = ''
    bindkey -v

    # currently required due to https://github.com/LnL7/nix-darwin/issues/373
    autoload -U compinit && compinit
    # compdef cargo
    # source $(rustc --print sysroot)/share/zsh/site-functions/_cargo

    # ulimit -n 200000
    # ulimit -u 4096

    zstyle ':completion:*:*:*:*:*' menu select

    setopt APPEND_HISTORY
    setopt EXTENDED_HISTORY
    setopt HIST_EXPIRE_DUPS_FIRST
    setopt HIST_IGNORE_DUPS
    setopt HIST_FIND_NO_DUPS
    setopt HIST_REDUCE_BLANKS
    setopt HIST_IGNORE_SPACE

    alias ssh='TERM=xterm-256color ssh'
    alias socks4proxy='ssh -D 8888 -f -C -q -N'
    alias k='kubectl'
    alias l='exa -alF'
    alias ts='tmux new-session -n main -s'
    alias ta='tmux attach -t'
    alias tl='tmux list-sessions'
    alias em='em.zsh'
    alias doom='~/.emacs.d/bin/doom'
    alias kv="kubectl -n ves-system"
    if command -v exa >/dev/null; then
      alias l='exa -alF'
    else
      alias l='ls -alFG'
    fi

    if command -v em.zsh >/dev/null; then
      export EDITOR=em.zsh
      export VISUAL=em.zsh
    else
      export EDITOR=nvim
      export VISUAL=nvim
    fi
    export SAVEHIST="5000"
    export HISTSIZE="100000"
    export LC_ALL="en_US.UTF-8"
    export LANG="en_US.UTF-8"
    export LANGUAGE="en_US.UTF-8"
    export GOPATH="$HOME/gocode"
    export GO111MODULE="on"
    export BAT_THEME="1337"
    export LESS="-F -i -M -R -X --incsearch"
    export FZF_DEFAULT_OPTS="--info=inline --layout=default --tac --no-sort"
    export FZF_CTRL_R_OPTS="--sort"
    export SAML2AWS_USER_AGENT="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0"

    if [ -n "$''${commands[fzf-share]}" ]; then
      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"
    fi

    eval "$(zoxide init zsh)"
    eval "$(direnv hook zsh)"
    source <(kubectl completion zsh)
    export PATH=~/.local/bin:~/.cargo/bin:$PATH
    export PATH=$PATH:${pkgs.nodejs}/bin

    printf '\e]2;'$(hostname)'\a'
  '';
}
