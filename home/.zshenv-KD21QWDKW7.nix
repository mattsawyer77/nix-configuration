#!/usr/bin/env zsh

nix-config-check() {
  darwin-rebuild check --flake ~/workspaces/nix-configuration
}

nix-config-switch() {
  sudo echo -n \
    && darwin-rebuild switch --flake ~/workspaces/nix-configuration
}

nix-config-update() {
  (cd ~/workspaces/nix-configuration \
    && nix flake update)
}

check-color() {
  awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
  }'
}

launchctl-restart() {
  if [[ -v 1 ]]; then
    pattern="$1"
    services=($(launchctl list | pcregrep "$pattern" | awk '{print $3}'))
    for service in $services; do
      plist=($(find /Library/Launch* ~/Library/LaunchAgents -name "${service}.plist" | head -1 || :))
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
      && for import in $(grep -hE '^\s*import\s".*\.proto";' *.proto | cut -d'"' -f2); do \
        if [[ -z "${import_paths[$import]}" ]]; then
          echo >&2 "cache miss for $import"
          paths=$(fd --follow --full-path --glob --no-ignore-vcs "**/$import" \
            $(realpath --relative-to . $(git rev-parse --show-toplevel)) \
            | sd "/$import" "" \
            | sd '^(.+)$' '    \"$1\"')
          if [[ -z "$paths" ]]; then
            echo >&2 "could not find proto files for $import"
          fi
          import_paths+=( ["$import"]="$paths" )
        else
          echo >&2 "cache hit for ${import}: ${import_paths[$import]}"
        fi
        echo "${import_paths[$import]}"
      done \
    | sort -u)
  echo "$dirs"
}

# generate-protoc-import-dir-locals() {
#   local root_dir
#   local single_proto=""
#   if [[ ! -v 1 ]]; then
#     root_dir=.
#   else
#     if [[ -d "$1" ]]; then
#       root_dir="$1"
#     elif [[ -f "$1" ]]; then
#       single_proto="$1"
#       root_dir=$(dirname "$single_proto")
#     else
#       echo >&2 "$1 is neither a file nor a directory"
#       return 1
#     fi
#   fi
#   local repo_proto_dirs
#   if [[ -n "$single_proto" ]]; then
#     repo_proto_dirs=($root_dir)
#     echo "generating .dir-locals.el for $single_proto in ${root_dir} (non-recursive)..."
#   else
#     repo_proto_dirs=($(fd '\.proto$' "$root_dir" -x dirname | sort -u))
#     echo "generating .dir-locals.el for each directory recursively from ${root_dir}..."
#   fi
#   for repo_proto_dir in $repo_proto_dirs; do
#     import_dirs=$(find-proto-import-path "$repo_proto_dir")
#     if [[ -n "$import_dirs" ]]; then
#       echo " - generating ${repo_proto_dir}/.dir-locals.el"
#       cat >"${repo_proto_dir}/.dir-locals.el" <<EOF
# ((protobuf-mode .
#   ((flycheck-protoc-import-path .
#     (
# $import_dirs
#     )))))
# EOF
#     else
#       echo " - no import dirs found for $repo_proto_dir"
#     fi
#   done
# }

# generate-protoc-import-dir-locals() {
#   local root_dir=$(git rev-parse --show-toplevel)
#   repo_name=$(echo "$root_dir" | pcregrep -o '[^/]+/?$')
#   repo_proto_dirs=($(fd --no-ignore-vcs '\.proto$' "$root_dir" -x dirname | grep -v vendor | sort -u))
#   declare -a absolute_proto_roots
#   absolute_proto_roots=(
#     ${HOME}/.local/share/protobuf-extras
#     ${HOME}/.local/share/protobuf-extras/protobuf
#     ${root_dir}/proto
#     ${root_dir}/schema
#     ${root_dir}/schema/vendor
#     ${root_dir}/schema/vendor/github.com/gogo/googleapis
#   )
#   declare -a found_proto_roots
#   for dir in $absolute_proto_roots; do
#     if [[ -d "$dir" ]]; then
#       found_proto_roots+="$dir"
#     fi
#   done
#   echo "generating .dir-locals.el for each directory recursively from ${root_dir}..."
#   for repo_proto_dir in $repo_proto_dirs; do
#     (cd $repo_proto_dir && \
#       declare -a rel_proto_roots && \
#       for dir in $found_proto_roots; do
#         rel_proto_root=$(realpath --relative-to=. "$dir") && \
#           rel_proto_roots+="\"$rel_proto_root\""
#       done && \
#       echo " - generating ${repo_proto_dir}/.dir-locals.el" && \
#       cat >"${repo_proto_dir}/.dir-locals.el" <<EOF
# ((protobuf-mode .
#   ((flycheck-protoc-import-path .
#     (
# $rel_proto_roots
#     )))))
# EOF
#     )
#   done
# }

generate-protoc-import-dir-locals() {
  local username=$(whoami)
  local repo_dir=$(git rev-parse --show-toplevel)
  declare -a rel_proto_roots
  for subdir in $(find * \( -wholename proto -o -wholename schema -o -wholename '_excschema/schema/proto' -o -wholename '_extschema/*/schema' \)); do
    dir="${repo_dir}/${subdir}"
    if [[ -d "$dir" ]]; then
      rel_proto_roots+="$dir"
    fi
  done
  cat >"${repo_dir}/.dir-locals.el" <<EOF
((protobuf-mode .
  ((flycheck-protoc-import-path .
  (
    "/Users/${username}/.local/share/protobuf-extras"
    "/Users/${username}/.local/share/protobuf-extras/protobuf"
    $(printf '"%s"\n    ' "$rel_proto_roots[@]")
  )))))
EOF
}

ssm() {
  local instances=$(aws ec2 describe-instances --output json \
      | jq -r '.Reservations[].Instances[]|select(.State.Name=="running")' \
      2>/dev/null)
  if [[ $? -eq 0 ]] && [[ -n "$instances" ]]; then
    instance_id=$(echo "$instances" \
        | jq -r '{InstanceId,Tags:.Tags|from_entries}|"\(.Tags.Name): \(.InstanceId)"' \
        2>/dev/null \
      | sort \
      | INSTANCES=$instances fzf \
        --preview-window "right:60%" \
        --preview $'ec2-instance-info $INSTANCES {2}' \
      | awk '{print $2}')
    if [[ -n "$instance_id" ]]; then
      aws ssm start-session --target $instance_id
    else
      echo "no instance selected."
    fi
  else
    echo "no instances found."
  fi
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
      echo -n "delete branch ${branch} [y/n/d(iff)/q(uit)]? "
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

acr-login() {
  az login --tenant ves.f5.com && az acr login --name volterra && skopeo-acr-login
}

# find an image with the given tag name (i.e. name is equal to the git commit hash or branch name)
acr-find-tag() {
  local repo=$1
  if [[ -z "$repo" ]]; then
    echo >&2 "error: must specify repo and git ref to search for"
    return 1
  fi
  shift
  local name=$1
  if [[ -z "$name" ]]; then
    echo >&2 "error: must specify git ref to search for"
    return 1
  fi
  shift
  # try to transform branch name to pattern used to publish to ACR
  name=$(echo "$name" | sd '\W' '-' | sd '\-$' '' | tr '[:upper:]' '[:lower:]')
  az acr repository show-tags --name volterra --repository "ves.io/${repo}" --detail --orderby time_desc --query "[?contains(name,'${name}')]" $@
}

# find an image with the given tag name (i.e. name is equal to the git commit hash or branch name)
acr-find-digest() {
  local repo=$1
  if [[ -z "$repo" ]]; then
    echo >&2 "error: must specify repo and git ref to search for"
    return 1
  fi
  shift
  local digest=$1
  if [[ -z "$digest" ]]; then
    echo >&2 "error: must specify image digest to search for"
    return 1
  fi
  shift
  digest=$(echo "$digest" | pcregrep -o 'sha256:[0-9a-f]{40}')
  if [[ -z "$digest" ]]; then
    echo >&2 "error: invalid digest"
    return 1
  fi
  az acr repository show-tags --name volterra --repository "ves.io/${repo}" --detail --orderby time_desc --query "[?digest=='${digest}']" $@
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
    local container="${3:-$name}"
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
    local branch=${2:-master}
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
    local branch=${2:-master}
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
  local image="${1:-$(pcregrep -o 'volterra.*go-builder:v?[\d\.]+' .gitlab-ci.yml)}"
  if [[ -z "$image" ]]; then
    echo >&2 "ERROR: no image specified, and go-builder image could not be derived from .gitlab-ci.yml"
  else
    local cmd="${2:-bash}"
    local temp_passwd_file="$(mktemp)"
    local project_root="$(git rev-parse --show-toplevel)"
    local go_src_dir=$(echo $project_root | sed "s^.*src/\(.*\)^src/\1^")
    local go_cache_dir="${project_root}/.cache-docker/go"
    echo "running container shell with $image"
    echo "${USER}:x:${UID}:${GID}:Volterra User:${HOME}:/bin/bash" > "$temp_passwd_file" \
      && mkdir -p "${project_root}/.cache.go" \
      && docker run --rm -it --user ${UID}:${GID} --userns=host \
        --env GOCACHE=${go_cache_dir} \
        --env GOPATH=${GOPATH} \
        --env HOME=${HOME} \
        --env PS1="${image}:\w> " \
        --env DOCKER_IMAGE="$image" \
        --net host \
        -v ${HOME}/.cache:${HOME}/.cache \
        -v "$temp_passwd_file":/etc/passwd:ro,Z \
        -v ${PWD}:/go/${GOPATH}/src:Z \
        -v ${HOME}/.gitconfig:${HOME}/.gitconfig:ro \
        -v ${HOME}/.ssh:${HOME}/.ssh:ro \
        -v ${GOPATH}:${GOPATH} \
        -v ${project_root}:${project_root} \
        -v "${HOME}/.bashrc":"${HOME}/.bashrc" \
        -w ${project_root} \
        ${image} ${cmd}
    test -f "$temp_passwd_file" && rm -f "$temp_passwd_file"
  fi
}

# use after some long-running process to notify you while your
# brain's executive function is hyperfocusing on something else
notify() {
  exit_code=$?
  current_cl=${history[$HISTCMD]}
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
  GRPC_TLS_PORT=${STREAK_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap streak-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
  # echo "streak GRPC TLS port: $GRPC_TLS_PORT" >&2
  SERVER_CN=${STREAK_SERVER_CN:-$(kubectl -n ves-system get statefulset streak -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
  POD_NAME=${STREAK_POD:-"streak-0"}
  # echo "streak CN: $SERVER_CN" >&2
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    kubectl -n ves-system -c streak exec -it "$POD_NAME" -c streak -- \
      streakctl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
      $@
  else
    echo "could not determine streak's GRPC TLS port"
  fi
}

mauricectl() {
  GRPC_TLS_PORT=${MAURICE_GRPC_TLS_PORT:-6002}
  echo "maurice GRPC TLS port: $GRPC_TLS_PORT" >&2
  SERVER_CN=${MAURICE_SERVER_CN:-maurice.ves-system.svc.cluster.local}
  echo "maurice CN: $SERVER_CN" >&2
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    maurice_pod=$(kubectl -n ves-system get pods | grep maurice | awk '{print $1}')
    if [[ -n "$maurice_pod" ]]; then
      kubectl -n ves-system -c maurice exec -it "$maurice_pod" -c maurice -- \
        mauricectl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
        $@
    else
      echo "could not find a running maurice pod" >&2
    fi
  else
    echo "could not determine maurice's GRPC TLS port"
  fi
}

vulpixctl() {
  GRPC_TLS_PORT=${VULPIX_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap vulpix-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
  SERVER_CN=${VULPIX_SERVER_CN:-$(kubectl -n ves-system get deployment vulpix -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    kubectl -n ves-system -c vulpix exec -it deploy/vulpix -c vulpix -- \
      vulpixctl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
      $@ \
      | pcregrep -v 'WARNING|golang/protobuf|which has long been excluded'
  else
    echo "could not determine vulpix's GRPC TLS port"
  fi
}

griffinctl() {
  GRPC_TLS_PORT=${GRIFFIN_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap griffin-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
  echo "griffin GRPC TLS port: $GRPC_TLS_PORT" >&2
  SERVER_CN=${GRIFFIN_SERVER_CN:-$(kubectl -n ves-system get deployment griffin -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
  echo "griffin CN: $SERVER_CN" >&2
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    griffin_pod=$(kubectl -n ves-system get pods | grep griffin | awk '{print $1}')
    if [[ -n "$griffin_pod" ]]; then
      kubectl -n ves-system -c griffin exec -it "$griffin_pod" -c griffin -- \
        griffinctl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
        $@
    else
      echo "could not find a running griffin pod" >&2
    fi
  else
    echo "could not determine griffin's GRPC TLS port"
  fi
}

akarctl() {
  GRPC_TLS_PORT=${AKAR_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap akar-config -o json | jq -r '.data."config.yml"' | yq e '.GrpcTLSPort' -)}
  SERVER_CN=${AKAR_SERVER_CN:-$(kubectl -n ves-system get deployment akar -o json | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames' | cut -d',' -f1)}
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    akar_pod=$(kubectl -n ves-system get pods -lname=akar | awk '{print $1}')
    if [[ -n "$akar_pod" ]]; then
      kubectl -n ves-system -c akar exec -it "$akar_pod" -c akar -- \
        akard client-ctl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
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
  GRPC_TLS_PORT=${NIO_GRPC_TLS_PORT:-$(kubectl -n ves-system get configmap "${service_name}-config" -o json \
    | jq -r '.data."nio.yml"' \
    | yq e '.GrpcTLSPort' -)}
  # echo "nio GRPC TLS port: $GRPC_TLS_PORT" >&2
  SERVER_CN=${NIO_SERVER_CN:-$(printf '%s\n' "$service" \
    | jq -r '.spec.template.spec.containers[]|select(.name=="wingman")|.env|from_entries|.serviceNames|split(",")|last')}
  # echo "nio CN: $SERVER_CN" >&2
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    nio_pod=$(kubectl -n ves-system get pods --no-headers -l name="${service_name}" | awk '{print $1}' | head -1)
    if [[ -n "$nio_pod" ]]; then
      kubectl -n ves-system exec -it "$nio_pod" -c nio -- \
        nioctl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
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
  GRPC_TLS_PORT=${VEGA_GRPC_TLS_PORT:-$(kubectl -n ves-system describe cm ver-config | grep -E '^GrpcTLSPort' | cut -d' ' -f2)}
  SERVER_CN=${VEGA_SERVER_CN:-$(kubectl -n ves-system describe cm ver-config | grep VegaCommonName | cut -d' ' -f2)}
  # echo "vega CN: $SERVER_CN" >&2
  if [[ -n "$GRPC_TLS_PORT" ]]; then
    vega_pod=$(kubectl -n ves-system get pods --no-headers -l "app=ver,cfg!=ver" | awk '{print $1}' | head -1)
    if [[ -n "$vega_pod" ]]; then
      kubectl -n ves-system exec -it "$vega_pod" -c vega -- \
        vegactl -u "localhost:${GRPC_TLS_PORT}" --server-cn "$SERVER_CN" \
        $@
    else
      echo "could not find a running vega pod" >&2
    fi
  else
    echo "could not determine vega's GRPC TLS port"
  fi
}

env-site-fqdn() {
  if [ ! -v 1 ]; then
    echo >&2 "must specify environment (demo1, crt, staging, prod)"
    return 1
  fi
  local environment="$1"
  local site="${2:-gc01}"
  case $environment in
    demo1)
      site_fqdn="${site}.int.ves.io"
      ;;
    crt)
      site_fqdn="${site}.int.ves.io"
      ;;
    staging)
      if [[ "$site" == "gc01" ]]; then
        site=gc1-iad-01
      fi
      site_fqdn="${site}.int.volterra.us"
      ;;
    prod)
      if [[ "$site" == "gc01" ]]; then
        site=gc01-cle
      fi
      site_fqdn="${site}.int.ves.io"
      ;;
    *)
      echo >&2 "unknown environment $environment"
      return 1
      ;;
  esac
  echo "$site_fqdn"
}

env-compass-hostname() {
  if [ ! -v 1 ]; then
    echo >&2 "must specify environment (demo1, crt, staging, prod)"
    return 1
  fi
  local environment="$1"
  case $environment in
    demo1)
      compass_hostname="compass-lma.demo1.volterra.us"
      ;;
    crt)
      compass_hostname="compass-lma.crt.volterra.us"
      ;;
    staging)
      compass_hostname="compass-lma.staging.volterra.us"
      ;;
    prod)
      compass_hostname="compass-lma.ves.volterra.io"
      ;;
    *)
      echo >&2 "unknown environment $environment"
      return 1
      ;;
  esac
  echo "$compass_hostname"
}

# for reference, the following was sometimes used instead of /ves.io.stdlib/introspect/read/pprof_profile, not sure of the exact implications
# url="https://${compass_hostname}/introspection/${site_fqdn}/${service}/debug/pprof/${pprof_type}?debug=1&seconds=${sample_time}"
profile-service() {
  usage="usage:\nprofile_service service_name [environment] [site] [pprof_type] [sample_time]"
  if [ ! -v 1 ]; then
    echo >&2 "must specify service to profile"
    echo >&2 "$usage"
    return 1
  fi
  local service="$1"
  local environment="${2:-demo1}"
  local site="${3:-gc01}"
  local pprof_type="${4:-cpu}"
  local valid_profile=false
  for t in cpu block mutex goroutine heap allocs; do
    if [[ "$pprof_type" == "$t" ]]; then
      valid_profile=true
    fi
  done
  if [[ "$valid_profile" == "false" ]]; then
    echo >&2 "invalid pprof profile type, valid types are cpu, block, mutex, goroutine, heap, or allocs"
    return 1
  fi
  local sample_time="${5:-30}"
  local site_fqdn=$(env-site-fqdn "$environment" "$site")
  local compass_hostname=$(env-compass-hostname "$environment")
  output_filename_base="${service}-${site}.${environment}.${pprof_type}.$(date --iso-8601=seconds)"
  url="https://${compass_hostname}/introspection/${site_fqdn}/${service}/ves.io.stdlib/introspect/read/pprof_profile?name=${pprof_type}&debug_mode=1&seconds=${sample_time}"
  echo "starting profile of $service on $environment (url: $url) for ${sample_time}s..."
  curl \
    --insecure \
    --fail-with-body \
    --no-progress-meter \
    --cert-type P12 \
    --cert "$HOME/.ves-internal/${environment}/usercerts.p12:volterra" \
    -o "${output_filename_base}.json" \
    "$url" \
      && cat "${output_filename_base}.json" | jq -r '.contents' | base64 --decode > "${output_filename_base}.pprof" \
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
  echo "updating image for ${resource_type}/${resource_name} to sha256:${image_sha}..."
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
  host=$(echo "$@" | grep -oE 'compass-lma\.(\w+)\.volterra\.(us|io)')
  environment=$(echo "$host" | cut -d'.' -f2)
  if [[ "$environment" == "ves" ]]; then
    environment="prod"
  fi
  user_cert="${HOME}/.ves-internal/${environment}/usercerts.p12"
  if [[ -f "$user_cert" ]]; then
    curl --insecure --fail --no-progress-meter --cert-type P12 --cert "${user_cert}:volterra" "$@"
  else
    echo >&2 "unknown environment $environment"
    return 1
  fi
}

# Service Introspection C(LI)
# https://compass-lma.demo1.volterra.us/introspection/gc01.int.ves.io/maurice/ves.io.stdlib/introspect/read/object/ves.io.maurice.site.Object?response_format=1&tenant_filter=compass&page_start=0&page_limit=1000
# https://compass-lma.demo1.volterra.us/introspection/gc01.int.ves.io/maurice/ves.io.stdlib/introspect/read/object/ves.io.maurice.site.Object/3395c300-35c3-4e31-b733-c10607ab780e
# https://compass-lma.demo1.volterra.us/introspection/sawyer-osm-gcp-m.testcorp-hagrmdbk.tenant.int.ves.io/piku/ves.io.stdlib/introspect/read/object/ves.io.piku.oper.bgp_state.Object?response_format=1&page_start=0&page_limit=1000
sic() {
  local usage="sic <environment> <service> <object type> [<object UID>] [--tenant <tenantname>]\nwhere environment is one of demo1, crt, staging, or prod"
  if ! command -v ijq >/dev/null; then
    echo >&2 "error: this requires ijq to be installed"
    return 1
  fi
  if [[ $# -lt 3 ]]; then
    echo >&2 $usage
    return 1
  fi
  local environment="$1"
  shift
  local service=$1
  shift
  local site=$(env-site-fqdn "$environment" "$service")
  local compass_hostname=$(env-compass-hostname "$environment")
  if echo "$service" | grep -E '.+/.+' >/dev/null; then
    site="$(echo $service | cut -d'/' -f1)"
    service="$(echo $service | cut -d'/' -f2)"
  fi
  local object_type=$1
  shift
  local uid=''
  local tenant=''
  if [[ -n "$1" ]] && [[ "$1" != "--tenant" ]]; then
    uid="$1"
    shift
  fi
  if [[ "$1" == "--tenant" ]] && [[ -n "$2" ]]; then
    tenant="$2"
  fi
  local user_cert="${HOME}/.ves-internal/${environment}/usercerts.p12"
  local url=''
  local initial_jq_query='.'
  if [[ -n "$uid" ]]; then
    url="https://${compass_hostname}/introspection/${site}/${service}/ves.io.stdlib/introspect/read/object/${object_type}/${uid}"
  else
    url="https://${compass_hostname}/introspection/${site}/${service}/ves.io.stdlib/introspect/read/object/${object_type}?response_format=1&page_start=0&page_limit=1000"
    if [[ -n "$tenant" ]]; then
      url="${url}&tenant_filter=${tenant}"
    fi
    initial_jq_query='.get_responses'
  fi
  if [[ -f "$user_cert" ]]; then
    response=$(curl --insecure --fail --no-progress-meter --cert-type P12 --cert "${user_cert}:volterra" "$url")
    if [[ $? == 0 ]]; then
      if [[ -z "$response" ]]; then
        echo >&2 "no response returned by url: $url"
        return 1
      fi
      err=$(printf '%s\n' "$response" | jq -r '.err.message')
      if [[ -n "$err" ]] && [[ "$err" != "null" ]]; then
        err_code=$(printf '%s\n' "$response" | jq -r '.err.code')
        if [[ -n "$err_code" ]]; then
          echo >&2 -n "${err_code}: "
        fi
        echo >&2 "$err"
        return 1
      fi
      printf '%s\n' "$response" | jq "$initial_jq_query" | ijq -r
    else
      echo >&2 "failed to query url: $url"
      return 1
    fi
  else
    echo >&2 "unknown environment $environment"
    return 1
  fi
}

get-latest-ce-version() {
  local environment="${1:-demo1}"
  local site_fqdn=$(env-site-fqdn "$environment")
  local compass_hostname=$(env-compass-hostname "$environment")
  curl "https://${compass_hostname}/introspection/${site_fqdn}/maurice/ves.io.stdlib/introspect/read/object/ves.io.pikachu.version.Object?response_format=1&page_start=0&page_limit=1000" \
    -H 'pragma: no-cache' \
    -H 'content-type: application/json' \
    -H 'accept: application/json' \
    -H 'cache-control: no-cache' \
    -H "authority: ${compass_hostname}" \
    --compressed \
    -sf \
    --cert-type P12 \
    --cert ~/.ves-internal/${environment}/usercerts.p12:volterra \
    | jq -r '.get_responses|to_entries[]|select(.value.object.metadata.name=="ce-build-version")|.value.object.spec.version_spec.name'
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
  if ! matrix certInfo >/dev/null 2>&1; then
    for env in demo1 crt staging prod; do matrix get-user-cert -b firefox -e "$env"; done
  else
    matrix certInfo 2>&1 \
      | pcregrep -B4 'status:.*expired' \
      | pcregrep -B2 '^\s*User Certificate' \
      | pcregrep -o '^\w+' \
      | xargs -I% sh -c "echo renewing cert for %...; matrix get-user-cert -b firefox -e %"
  fi
}

gc-login() {
  export USE_GKE_GCLOUD_AUTH_PLUGIN=True
  kubectl config use-context gke_devtest-293809_us-east4_gc01-int-ves-io
  k cluster-info 2>/dev/null \
    || (gcloud auth login --project=devtest-293809 \
    && gcloud container clusters get-credentials gc01-int-ves-io --region us-east4)
}

gc-crt-login() {
  export USE_GKE_GCLOUD_AUTH_PLUGIN=True
  kubectl config use-context gke_crt-env_us-east4_gc01-crt-int-ves-io
  k cluster-info 2>/dev/null \
    || (gcloud auth login --project=crt-env \
    && gcloud container clusters get-credentials gc01-crt-int-ves-io --region us-east4)
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

site-public-ips() {
  local environment
  local site
  local usage="usage:\nsite-public-ips [environment] site-name"
  if [ ! -v 1 ]; then
    echo >&2 "must specify site"
    echo >&2 "$usage"
    return 1
  fi
  if [ $# -eq 2 ]; then
    environment="$1"
    shift
  else
    environment=demo1
  fi
  local compass_hostname=$(env-compass-hostname "$environment")
  local site_fqdn=$(env-site-fqdn "$environment")
  site="$1"
  tf_params_objects=$(curl "https://${compass_hostname}/introspection/${site_fqdn}/vulpix/ves.io.stdlib/introspect/read/object/ves.io.vulpix.terraform_parameters.Object?response_format=1&page_start=0&page_limit=2000" \
    -H 'content-type: application/json' \
    -H 'accept: application/json' \
    -H "authority: $compass_hostname" \
    --compressed \
    --insecure \
    --silent \
    --fail \
    --cert-type P12 \
    --cert ${HOME}/.ves-internal/${environment}/usercerts.p12:volterra)
  if [[ $? -ne 0 ]]; then
    return $?
  fi
  tf_params_id=$(printf '%s\n' "$tf_params_objects" \
    | jq -r '.get_responses|to_entries[]|select(.value.object.system_metadata.owner_view.name == "'"$site"'")|.value.object.metadata.uid')
  if [[ $? -ne 0 ]]; then
    return $?
  fi
  if [[ -z "$tf_params_id" ]]; then
    echo >&2 "could not find terraform parameters for site $site"
    return 1
  fi
  tf_status=$(curl "https://${compass_hostname}/introspection/${site_fqdn}/streak/ves.io.streak/introspect/read/status-objects" \
    -H 'content-type: application/json' \
    -H 'accept: application/json' \
    -H 'authority: compass-lma.staging.volterra.us' \
    --data-binary '{"status_object_type":"ves.io.schema.views.terraform_parameters.StatusObject","config_object_uids":["'"$tf_params_id"'"]}' \
    --compressed \
    --insecure \
    --fail \
    --silent \
    --cert-type P12 \
    --cert ${HOME}/.ves-internal/${environment}/usercerts.p12:volterra)
  if [[ -z "$tf_status" ]]; then
    echo >&2 "could not find terraform status for site $site"
    return 1
  fi

  master_ips=$(printf '%s\n' "$tf_status" \
    | jq -r '.config_status_map|to_entries[].value.items[].status_object_bytes.apply_status.tf_output' | grep master_public_ip_address | pcregrep -o '([\d\.]+)$')
  if [[ -z "$master_ips" ]]; then
    # check for multi-node
    master_ips=$(printf '%s\n' "$tf_status" | grep -A3 master_public_ip_address | pcregrep -o '\d+\.\d+\.\d+\.\d+' | pcregrep -v '^(10\.|192.168\.|172\.16\.)')
  fi
  if [[ -z "$master_ips" ]]; then
    echo >&2 "could not find public IPs for site $site, terraform apply status:"
    printf '%s\n' "$tf_status" >&2
    return 1
  fi
  printf '%s\n' "$master_ips"
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
  config_objects_str="[$(print -R ${(j|,|)config_object_uids})]"
  streakctl customAPI ves.io.streak.CustomAPI ListStatusObjects --json-data '{ "status_object_type": "'"$status_object_type"'", "config_object_uids": '"$config_objects_str"' }' | yq e '.'
}

# alacritty-toggle-theme() {
#   local dark_theme="Ocean.dark"
#   local light_theme="Ocean.light"
#   if ! command -v alacritty-themes >/dev/null; then
#     echo -n 'alacritty-themes is not installed. install it now? [y/n]? '
#     read -r answer
#     if [[ "$answer" == "y" ]]; then
#       sudo npm install -g alacritty-themes && alacritty-toggle-theme $@
#     fi
#   fi
#   if [[ $(alacritty-themes --current) == "$dark_theme" ]]; then
#     alacritty-themes "$light_theme"
#   else
#     alacritty-themes "$dark_theme"
#   fi
# }

get-service-certificate() {
  port_forward_pid=''
  temp_cert_file=''
  _cert_cleanup() {
    local pid=$1
    local temp_file=$2
    echo >&2 "cleaning up..."
    if [[ -n "$pid" ]] && ps -fp "$pid" >/dev/null; then
      echo >&2 "killing port forward process with pid $pid"
      kill "$port_forward_pid" >/dev/null || :
    fi
    if [[ -n "$temp_file" ]] && [[ -f "$temp_file" ]]; then
      echo >&2 "cleaning up temp cert file $temp_file"
      rm -f "$temp_file" >/dev/null || :
    fi
  }
  trap '_cert_cleanup $port_forward_pid $temp_cert_file; unset -f _cert_cleanup' EXIT
  if [ ! -v 1 ]; then
    echo >&2 "must specify a service"
    return 1
  fi
  service="$1"
  if ! kubectl cluster-info >/dev/null; then
    echo >&2 "you must login to the cluster"
    return 1
  fi
  alias kv='kubectl -n ves-system'
  proxy_url=$(kv get cm "${service}-config" -o yaml \
    | yq e '.data|to_entries|.0.value' \
    | yq e '.tls.serverParams.tlsCertificates[0]'.certificateUrl \
    | sd 'wingman://' 'http://')
  pod_name=$(kv get pods -l"name=${service}" --no-headers | awk '{print $1}' | head -1)
  if [[ $? -ne 0 ]]; then
    echo 2>&1 "failed to get a pod named ${service}"
    return 1
  fi
  wingman_port=$(echo "$proxy_url" \
    | pcregrep -o 'http://[^:/]+:(\d+)' \
    | cut -d':' -f3)
  if [[ -z "$wingman_port" ]]; then
    echo >&2 "could not parse wingman port from '${proxy_url}'"
    return 1
  fi
  echo >&2 "port-forwarding to ${pod_name} 8070:${wingman_port}"
  kv port-forward "$pod_name" "8070:${wingman_port}" >/dev/null & port_forward_pid=$!
  if [[ $? -ne 0 ]]; then
    echo >&2 "could not port-forward to ${pod_name} port ${wingman_port}"
    return 1
  fi
  echo >&2 "requesting cert at url ${proxy_url}"
  temp_cert_file="$(mktemp).crt"
  sleep 1
  echo "downloading cert to $temp_cert_file"
  curl "$proxy_url" --no-progress-meter --fail -o "$temp_cert_file" \
    && openssl x509 -in "$temp_cert_file" -text -noout -subject
}

# find emacs-overlay's latest successful hydra job and get the emacs-overlay src commit for it to use as input for the flake
hydra-emacs-overlay-revision() {
 jobset_eval_id=$(curl -sf --location --header "Accept: application/json" 'https://hydra.nix-community.org/jobset/emacs-overlay/stable/latest-finished' | jq -r '.jobsetevals|first')
 if [[ -z "$jobset_eval_id" ]]; then
   echo >&2 "error: could not retrieve jobset info via 'https://hydra.nix-community.org/jobset/emacs-overlay/stable/latest-finished'"
   return 1
 else
   curl -sf --location --header "Accept: application/json" 'https://hydra.nix-community.org/jobset/emacs-overlay/stable/evals' | jq -r ".evals[]|select(.id==${jobset_eval_id})|.jobsetevalinputs.src.revision"
 fi
}

set-input-volume-percent() {
  if [[ ! -v 1 ]]; then
    echo >&2 "error: must specify an input volume percentage (0-100)"
    return 1
  fi
  input_volume="$1"
  if echo "$input_volume" | pcregrep '\D'; then
    echo >&2 "error: invalid input volume: $input_volume"
    return 1
  fi
  osascript \
    -e 'tell application "System Events"' \
    -e "set volume input volume $input_volume" \
    -e 'end tell'
}

# NOTE:
# - input has no `muted` flag, it's simply a volume of 0
# - store previous unmuted volume level in ~/.local/share/input-volume
toggle-audio-input-mute() {
  mkdir -p ~/.local/share
  local default_input_volume=87
  local prev_audio_input_volume_file=~/.local/share/input-volume
  current_input_volume=$(osascript \
    -e 'tell application "System Events"' \
    -e 'get volume settings' \
    -e 'end tell' \
    | sd ',' '\n' \
    | grep 'input volume:' \
    | cut -d':' -f2)
  if [[ -z "$current_input_volume" ]]; then
    echo >&2 "error: could not determine current input volume"
    return 1
  fi
  if echo "$current_input_volume" | pcregrep '\D'; then
    echo >&2 "error: current input volume is unexpected: $current_input_volume"
    return 1
  fi
  if [[ $current_input_volume -eq 0 ]]; then
    if [[ -f $prev_audio_input_volume_file ]]; then
      set-input-volume-percent $(cat $prev_audio_input_volume_file)
    else
      set-input-volume-percent $default_input_volume
    fi
  else
    echo -e $current_input_volume > $prev_audio_input_volume_file
    set-input-volume-percent 0
  fi
}

png2icns() {
  local png_file=$1
  if [[ -z "$png_file" ]]; then
    echo >&2 "must specify a png filename"
    return 1
  fi
  file_info=$(file "$png_file")
  if ! (echo "$file_info" | grep 'PNG image data, 512 x 512' >/dev/null) && ! (echo "$file_info" | grep 'PNG image data, 1024 x 1024' >/dev/null); then
    echo >&1 "$png_file does not appear to be a valid 512 x 512 or 1024 x 1024 PNG file"
    return 1
  fi
  local icns_file=$(echo "$png_file" | sd '\.([^\.]+)$' '.icns')
  local temp_dir=$(mktemp -d)
  sips -z 16 16     "${png_file}" --out ${temp_dir}/icon_16x16.png \
  && sips -z 32 32     "${png_file}" --out ${temp_dir}/icon_16x16@2x.png \
  && sips -z 32 32     "${png_file}" --out ${temp_dir}/icon_32x32.png \
  && sips -z 64 64     "${png_file}" --out ${temp_dir}/icon_32x32@2x.png \
  && sips -z 128 128   "${png_file}" --out ${temp_dir}/icon_128x128.png \
  && sips -z 256 256   "${png_file}" --out ${temp_dir}/icon_128x128@2x.png \
  && sips -z 256 256   "${png_file}" --out ${temp_dir}/icon_256x256.png \
  && sips -z 512 512   "${png_file}" --out ${temp_dir}/icon_256x256@2x.png \
  && sips -z 512 512   "${png_file}" --out ${temp_dir}/icon_512x512.png
  cp "${png_file}" ${temp_dir}/icon_512x512@2x.png
  iconutil --convert icns ${temp_dir} -o "$icns_file" \
    && echo "converted $png_file to $icns_file"
  rm -R ${temp_dir}
}

# addr="http://localhost:3100"
#                         Server address. Can also be set using LOKI_ADDR
#                         env var.
# --username=""           Username for HTTP basic auth. Can also be set
#                         using LOKI_USERNAME env var.
# --password=""           Password for HTTP basic auth. Can also be set
#                         using LOKI_PASSWORD env var.
# --ca-cert=""            Path to the server Certificate Authority. Can also
#                         be set using LOKI_CA_CERT_PATH env var.
# --tls-skip-verify       Server certificate TLS skip verify. Can also be
#                         set using LOKI_TLS_SKIP_VERIFY env var.
# --cert=""               Path to the client certificate. Can also be set
#                         using LOKI_CLIENT_CERT_PATH env var.
# --key=""                Path to the client certificate key. Can also be
#                         set using LOKI_CLIENT_KEY_PATH env var.
# https://compass-lma.staging.volterra.us/analytics/api/datasources/proxy/2/loki/api/v1/label
# LOKI_ADDR="${compass_hostname}/analytics/api/datasources/proxy/2"
# LOKI_HTTP_PROXY_URL="${compass_hostname}/analytics/api/datasources/proxy/2" \
loki() {
  usage="usage: loki environment logcli-args\n  where\n    environment is one of demo1, crt, staging, prod, etc.\n    logcli-args are arguments to logcli (see logcli --help for more info)"
  if [[ $# -lt 2 ]]; then
    echo >&1 "$usage"
    return 1
  fi
  local environment="$1"
  shift
  if [[ $# -lt 1 ]]; then
    echo >&1 "$usage"
    return 1
  fi
  local compass_hostname=$(env-compass-hostname "$environment")
  if [[ $? -ne 0 ]]; then
    return 1
  fi
  LOKI_CA_CERT=${HOME}/.ves-internal/${environment}/cacerts/server_ca_with_compass.crt \
    LOKI_CLIENT_CERT_PATH=${HOME}/.ves-internal/${environment}/usercerts.crt \
    LOKI_CLIENT_KEY_PATH=${HOME}/.ves-internal/${environment}/usercerts.key \
    LOKI_TLS_SKIP_VERIFY=1 \
    LOKI_ADDR="https://${compass_hostname}/analytics/api/datasources/proxy/2" \
    logcli $@
}

setup-ce() {
  local environment
  local site
  local usage="usage:\nsetup-ce [environment] site-name"
  if [ ! -v 1 ]; then
    echo >&2 "must specify site"
    echo >&2 "$usage"
    return 1
  fi
  if [ $# -eq 2 ]; then
    environment="$1"
    shift
  else
    environment=demo1
  fi
  local compass_hostname=$(env-compass-hostname "$environment")
  local site_fqdn=$(env-site-fqdn "$environment")
  site="$1"
  # check that ssh works
  echo "checking public IPs for ${site}..."
  export IFS=$'\n'
  public_ips=($(site-public-ips $environment $site))
  for ip in $public_ips; do
    echo "testing ssh to ${site} at ${ip}..."
    if ! ssh -nq -i ~/.ves-internal/$environment/id_rsa "vesop@${ip}" >/dev/null; then
      echo >&2 "ssh to $ip failed"
      return 1
    fi
    for file in ~/onedrive/ce-files/*; do
      echo "copying $(basename ${file}) to ${site} at ${ip}..."
      if ! scp -q -i "~/.ves-internal/$environment/id_rsa" "$file" "vesop@${ip}:"; then
        echo >&2 "scp of $file to $ip failed"
        return 1
      fi
    done
    echo "installing remote files..."
    ssh -q -i "~/.ves-internal/$environment/id_rsa" "vesop@${ip}" -o RemoteCommand="sudo cp -v plog /opt/bin/" || return 1
    ssh -q -i "~/.ves-internal/$environment/id_rsa" "vesop@${ip}" -o RemoteCommand="sudo cp -v .vimrc /root" || return 1
    ssh -q -i "~/.ves-internal/$environment/id_rsa" "vesop@${ip}" -o RemoteCommand="sudo cp -v .bashrc /root" || return 1
  done
  echo "CE $site setup successfully"
}

# -subj "/CN=example.com" -addext "subjectAltName=DNS:example.com,DNS:www.example.net,IP:10.0.0.1"
# print -R ${(pj|,|)domains}
# TODO: support ECDSA
generate-tls-cert() {
  local usage="usage:\ngenerate-tls-cert expiration-days domains...\n  (note: the first domain will be used as the CN and any subsequent domains will be used as SANs)"
  if [[ $# -lt 2 ]]; then
    echo >&2 "error: must specify expiration-days and at least one domain"
    echo >&2 $usage
    return 1
  fi
  local days="$1"
  shift
  local cn="$1"
  local subject="/CN=${cn}"
  shift
  local sans=''
  if [[ -n "$1" ]]; then
    sans="subjectAltName="
    while [[ $# -gt 0 ]]; do
      sans="${sans}DNS:${1}"
      shift
      if [[ -n "$1" ]]; then
        sans="${sans},"
      fi
    done
  fi
  local keyfile="${cn}.key"
  local crtfile="${cn}.crt"
  rm -rf "$keyfile" || :
  rm -rf "$crtfile" || :
  (\
    if [[ -n "$sans" ]]; then
      openssl req -x509 -newkey rsa:4096 -sha256 -days "$days" -nodes \
        -keyout "$keyfile" -out "$crtfile" -subj "$subject" \
        -addext "$sans"
    else
      openssl req -x509 -newkey rsa:4096 -sha256 -days "$days" -nodes \
        -keyout "$keyfile" -out "$crtfile" -subj "$subject"
    fi) \
  && certhash=$(openssl x509 -noout -modulus -in "$crtfile" | openssl md5) \
  && keyhash=$(openssl rsa -noout -modulus -in "$keyfile" | openssl md5) \
  && (\
    if [[ "$certhash" != "$keyhash" ]]; then
      echo >&2 "error: certificate hash does not match key hash"
      return 1
    fi) \
      && echo "certificate generated successfully:\n - certificate: $crtfile\n - key: $keyfile"
}

# kubectl exec [POD] [COMMAND] is DEPRECATED and will be removed in a future version. Use kubectl exec [POD] -- [COMMAND] instead.
k8s-dnsutils() {
  ns=${NS:-ves-system}
  if kubectl -n "$ns" get pod dnsutils >/dev/null 2>&1; then
    echo >&2 "error: dnsutils pod already running"
    return 1
  fi
  kubectl -n "$ns" apply -f <(cat<<EOF
apiVersion: v1
kind: Pod
metadata:
  name: dnsutils
  namespace: ${ns}
spec:
  containers:
  - name: dnsutils
    image: registry.k8s.io/e2e-test-images/jessie-dnsutils:1.3
    command:
      - sleep
      - "infinity"
    imagePullPolicy: IfNotPresent
  restartPolicy: Always
EOF
) \
  && kubectl -n "$ns" wait --for=condition=Ready pod/dnsutils \
  && kubectl -n "$ns" exec -it dnsutils -c dnsutils -- bash
  kubectl -n "$ns" delete pod dnsutils
}
