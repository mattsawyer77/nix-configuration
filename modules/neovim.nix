(self: super: {
  neovim = super.neovim.override {
    vimAlias = true;
    configure = {
      packages.myPlugins = with super.vimPlugins; {
        start = [
          ayu-vim
          # coc-diagnostic
          # coc-fzf
          # coc-go
          # coc-highlight
          # coc-json
          # coc-nvim
          # coc-rust-analyzer
          # coc-yaml
          edge
          git-blame-nvim
          gitgutter
          jellybeans-nvim
          lualine-nvim
          lush-nvim
          lsp_extensions-nvim
          neogit
          nvcode-color-schemes-vim
          nvim-compe
          nvim-dap
          nvim-dap-ui
          nvim-lspconfig
          nvim-tree-lua
          nvim-treesitter
          nvim-web-devicons
          oceanic-next
          onedark-vim
          onehalf
          plenary-nvim
          popup-nvim
          project-nvim
          rust-tools-nvim
          sonokai
          tcomment_vim
          telescope-nvim
          telescope-project-nvim
          tender-vim
          vim-go
          vim-nix
          vim-one
          vim-protobuf
          vim-surround
          vim-toml
          vim-yaml
        ];
        opt = [];
      };
      customRC = ''
        ${(builtins.readFile ./neovim/init.vim)}
        lua << EOF
        ${(builtins.readFile ./neovim/compe.lua)}
        ${(builtins.readFile ./neovim/lsp.lua)}
        ${(builtins.readFile ./neovim/lualine.lua)}
        ${(builtins.readFile ./neovim/rust-tools.lua)}
        ${(builtins.readFile ./neovim/telescope.lua)}
        ${(builtins.readFile ./neovim/tree-sitter.lua)}
        require'nvim-tree'.setup()
        vim.g.nvim_tree_respect_buf_cwd = 1
        require("nvim-tree").setup({
          update_cwd = true,
          update_focused_file = {
            enable = true,
            update_cwd = true
          },
        })
        require("project_nvim").setup()
EOF
      '';
    };
  };
})
