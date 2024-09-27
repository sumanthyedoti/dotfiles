return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    {
      "williamboman/mason.nvim",
      dependencies = {
        "williamboman/mason-lspconfig.nvim", -- bridges mason.nvim with the nvim-lspconfig
        "whoIsSethDaniel/mason-tool-installer",
      },
    },
    {
      "simrat39/symbols-outline.nvim",
      config = function()
        require("sumanthyedoti.plugins.lsp.symobol-outline")
      end,
    },
    { "nvimtools/none-ls.nvim", event = "BufEnter" }, -- for formatters and linters
    { "mfussenegger/nvim-lint" },
    { "stevearc/conform.nvim" },
    "jose-elias-alvarez/typescript.nvim",
    "mfussenegger/nvim-jdtls",
    "b0o/schemastore.nvim",
    { "antosha417/nvim-lsp-file-operations", config = true },
    "nvim-telescope/telescope.nvim",
    "folke/which-key.nvim",
  },
  config = function()
    local wk = require("which-key")
    local lspconfig = require("lspconfig")
    local util = require("lspconfig/util")
    local cmp_nvim_lsp = require("cmp_nvim_lsp")
    local typescript = require("typescript")
    local utils = require("sumanthyedoti.utils")
    local map = utils.map_key

    -- Change the Diagnostic symbols in the sign column (gutter)
    local signs = {
      { name = "DiagnosticSignError", text = "" },
      { name = "DiagnosticSignWarn", text = "" },
      { name = "DiagnosticSignHint", text = "⚡" },
      { name = "DiagnosticSignInfo", text = "" },
    }

    for _, sign in ipairs(signs) do
      vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
    end

    local config = {
      virtual_text = false,
      -- show signs
      signs = {
        active = signs,
      },
      update_in_insert = true,
      underline = true,
      severity_sort = true,
      float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
      },
    }
    vim.diagnostic.config(config)
    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = "rounded",
    })
    vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
      border = "rounded",
    })

    -- enable keybinds only for when lsp server available
    local on_attach = function(client, bufnum)
      -- keybind options
      wk.register({
        g = {
          name = "Goto",
          -- d = { "<cmd>lua vim.lsp.buf.definition()<CR>", "Goto type definition" },
          d = { "<cmd>Telescope lsp_definitions<CR>", "Goto type definition" },
        },
      }, { prefix = "" })

      map("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", { desc = "Hover doc" })
      wk.register({
        l = {
          name = "LSP",
          a = { "<cmd>lua vim.lsp.buf.code_action()<CR>", "Code actions" },
          r = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
          f = { "<cmd>Telescope lsp_references<CR>", "Show LSP references" },
          -- i = { "<cmd>lua vim.lsp.buf.implementation()<CR>", "Go to implementation" },
          i = { "<cmd>Telescope lsp_implementations<CR>", "Show LSP implementations" },
          t = { "<cmd>lua vim.lsp.buf.hover()<CR>", "Hover doc" },
          T = { "<cmd>Telescope lsp_type_definitions<CR>", "Show LSP type definitions" },
          o = { "<cmd>SymbolsOutline<CR>", "Outline ↔" },
          d = { "<Cmd>lua vim.lsp.buf.declaration()<CR>", "Go to declaration" },

          --[[ Diagnostics ]]
          l = { "<cmd>lua vim.diagnostic.open_float()<CR>", "Line Diagnostics" },
          b = { "<cmd>Telescope diagnostics bufnr=0<CR>", "Buffer Diagnostics" },
          w = { "<cmd>Telescope diagnostics<CR>", "Workspace Diagnostics" },
          -- j = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Next Diagnostic" },
          -- k = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Previous Diagnostic" },

          --[[ Help ]]
          s = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature help" },

          --[[ Restart ]]
          R = { "<cmd>:LspRestart<CR>", "Restart LSP" },
        },
      }, { prefix = "<leader>", buffer = bufnum })
      map("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", { desc = "Next Diagnostic" })
      map("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", { desc = "Previous Diagnostic" })

      wk.register({
        lc = {
          name = "LSP Calls hierarchy",
          i = { "<cmd>lua vim.lsp.buf.incoming_calls()<CR>", "Incoming calls" },
          o = { "<cmd>lua vim.lsp.buf.outgoing_calls()<CR>", "Outgoing calls" },
        },
      }, { prefix = "<leader>", buffer = bufnum })
      -- typescript specific keymaps
      if client.name == "tsserver" then -- HERE: typescript LSP keymaps
        wk.register({
          lT = {
            name = "TypeScript", -- optional group name
            f = { ":TypescriptRenameFile<CR>", "Rename file" },
            o = { ":TypescriptOrganizeImports<CR>", "Organize imports" },
            x = { ":TypescriptRemoveUnused<CR>", "Remove unused" },
            i = { ":TypescriptAddMissingImports<CR>", "Add missing imports" },
            a = { ":TypescriptFixAll<CR>", "Fix all" },
          },
        }, { prefix = "<leader>", buffer = bufnum })
      end

      if client.name == "jdtls" then
        local jdtls = require("jdtls")
        wk.register({
          lJ = {
            name = "JDTLS", -- optional group name
            o = { jdtls.organize_imports, "Organize imports" },
            ev = { jdtls.extract_variable, "Organize imports" },
            ec = { jdtls.extract_constant, "Organize imports" },
            em = { ":lua require('jdtls').extract_method(true)<CR>", "Organize imports" },
          },
        }, { prefix = "<leader>", buffer = bufnum })
      end

      -- == cursor hover
      if client.server_capabilities.documentHighlight then
        vim.api.nvim_exec(
          [[
        augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END
        ]],
          false
        )
      end
    end

    -- used to enable autocompletion (assign to every lsp server config)
    local capabilities = cmp_nvim_lsp.default_capabilities()

    -- 🌐 https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
    -- configure html server
    lspconfig["bashls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "bash-language-server", "start" },
      filetypes = { "sh" },
    })

    lspconfig["html"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    lspconfig["emmet_ls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = {
        "html",
        "css",
        "sass",
        "scss",
        "svelte",
        "typescriptreact",
        "javascriptreact",
      },
    })

    --[[ configure typescript server with plugin ]]
    typescript.setup({
      capabilities = capabilities,
      filetypes = { "typescript", "typescriptreact", "typescript", "javascript", "javascriptreact", "astro" },
      server = {
        capabilities = capabilities,
        on_attach = on_attach,
      },
    })

    -- lspconfig.tsserver.setup({
    --   capabilities = capabilities,
    --   on_attach = on_attach,
    --   filetypes = { "typescript", "typescriptreact", "typescript", "javascript", "javascriptreact" },
    --   init_options = {
    --     preference = {
    --       disableSuggestions = true,
    --     },
    --   },
    --   cmd = { "typescript-language-server", "--stdio" },
    -- })

    lspconfig.ts_ls.setup({
      init_options = {
        plugins = {
          {
            name = "@vue/typescript-plugin",
            location = "/usr/local/lib/node_modules/@vue/typescript-plugin",
            languages = { "javascript", "typescript" },
          },
        },
      },
      cmd = { "typescript-language-server", "--stdio" },
      filetypes = {
        "javascript",
        "javascriptreact",
        "javascript.jsx",
        "typescript",
        "typescriptreact",
        "typescript.tsx",
        "astro",
      },
      root_dir = util.root_pattern("tsconfig.json", "jsconfig.json", "package.json", ".git"),
      single_file_support = true,
    })

    lspconfig.jsonls.setup({
      settings = {
        json = {
          schemas = require("schemastore").json.schemas({
            ignore = {},
          }),
          validate = { enable = true },
        },
      },
      filetypes = { "json" },
    })

    lspconfig.yamlls.setup({
      settings = {
        yaml = {
          schemaStore = {
            -- You must disable built-in schemaStore support if you want to use
            -- this plugin and its advanced options like `ignore`.
            enable = false,
            -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
            url = "",
          },
          schemas = require("schemastore").yaml.schemas(),
        },
      },
    })

    -- configure css server
    lspconfig["cssls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    lspconfig["tailwindcss"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "css", "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "heex" },
    })

    -- lspconfig["eslint"].setup({
    --   capabilities = capabilities,
    --   on_attach = function(client, bufnr)
    --     vim.api.nvim_create_autocmd("BufWritePre", {
    --       buffer = bufnr,
    --       command = "EslintFixAll",
    --     })
    --   end,
    -- })

    lspconfig["astro"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "astro" },
      cmd = { "astro-ls", "--stdio" },
      root_dir = util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git"),
    })

    -- configure lua server (with special settings)
    lspconfig["lua_ls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = { -- custom settings for lua
        Lua = {
          -- make the language server recognize "vim" global
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            -- make language server aware of runtime files
            library = {
              [vim.fn.expand("$VIMRUNTIME/lua")] = true,
              [vim.fn.stdpath("config") .. "/lua"] = true,
            },
          },
          telemetry = { enable = false },
          hint = { enable = true }, -- inlay hints
          completion = {
            callSnippet = "Replace",
          },
        },
      },
    })

    lspconfig.jdtls.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "java" },
      cmd = { "jdtls" },
      single_file_support = true,
      root_pattern = {
        -- Single-module projects
        {
          "build.xml", -- Ant
          "pom.xml", -- Maven
          "settings.gradle", -- Gradle
          "settings.gradle.kts", -- Gradle
        },
        -- Multi-module projects
        { "build.gradle", "build.gradle.kts" },
      } or vim.fn.getcwd(),
    })

    lspconfig.clojure_lsp.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "clojure-lsp" },
      filetypes = { "clojure", "edn" },
      root_pattern = { "project.clj", "deps.edn", "build.boot", "shadow-cljs.edn", ".git", "bb.edn" },
    })

    -- require("sumanthyedoti.plugins.lsp.elixir-tools")
    lspconfig.elixirls.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "elixir-ls" },
      flags = {
        debounce_text_changes = 150,
      },
      settings = {
        elixirLS = {
          dialyzerEnabled = false,
          fetchDeps = false,
        },
      },
    })

    lspconfig.gopls.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "gopls" },
      filetypes = { "go", "gomod" },
      root_dir = util.root_pattern("go.work", "go.mod", ".git"),
      settings = {
        gopls = {
          analyses = {
            unusedparams = true,
          },
          staticcheck = true,
        },
      },
    })

    lspconfig.hls.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "haskell", "lhaskell", "cabal" },
      root_dir = util.root_pattern("hie.yaml", "stack.yaml", "cabal.project", "*.cabal", "package.yaml"),
      single_file_support = true,
      settings = {
        haskell = {
          cabalFormattingProvider = "cabalfmt",
          formattingProvider = "ormolu",
        },
      },
    })

    lspconfig["pyright"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "pyright-langserver", "--stdio" },
      filetypes = { "python" },
    })

    lspconfig["clangd"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "clangd" },
      filetypes = { "c", "cpp" },
      offset_encoding = "utf-32",
    })

    lspconfig["prismals"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "prisma-language-server", "--stdio" },
      root_dir = util.root_pattern(".git", "package.json"),
      settings = {
        prisma = {
          prismaFmtBinPath = "",
        },
      },
    })

    lspconfig["rust_analyzer"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "rust-analyzer" },
      filetypes = { "rust" },
      root_dir = util.root_pattern("Cargo.toml", "rust-project.json"),
      settings = {
        ["rust-analyzer"] = {
          imports = {
            granularity = {
              group = "module",
            },
            prefix = "self",
          },
          cargo = {
            buildScripts = {
              enable = true,
            },
          },
          procMacro = {
            enable = true,
          },
        },
      },
    })

    lspconfig["terraformls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "terraform-ls", "serve" },
      filetypes = { "terraform", "terraform-vars" },
      root_dir = util.root_pattern(".terraform", ".git"),
    })

    lspconfig["solidity_ls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      cmd = { "solidity-language-server", "--stdio" },
      filetypes = { "solidity" },
      root_dir = util.root_pattern(".git", "package.json"),
    })

    require("sumanthyedoti.plugins.lsp.mason")
    -- require("sumanthyedoti.plugins.lsp.null-ls")
    -- require("sumanthyedoti.plugins.lsp.lint")
    require("sumanthyedoti.plugins.lsp.formatting")
  end,
}
