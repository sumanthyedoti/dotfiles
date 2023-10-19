Application.put_env(:elixir, :ansi_enabled, true)

timestamp = fn ->
  {_date, {hour, minute, _second}} = :calendar.local_time()

  [hour, minute]
  |> Enum.map(&String.pad_leading(Integer.to_string(&1), 2, "0"))
  |> Enum.join(":")
end

IEx.configure(
  # colors: [enabled: true],
  colors: [
    syntax_colors: [
      number: :light_yellow,
      atom: :light_cyan,
      string: :light_black,
      boolean: :red,
      nil: [:magenta, :bright]
    ],
    ls_directory: :cyan,
    ls_device: :yellow,
    doc_code: :green,
    doc_inline_code: :magenta,
    doc_headings: [:cyan, :underline],
    doc_title: [:cyan, :bright, :underline],
    eval_result: [:green, :bright],
    eval_error: [[:red, :bright, "Watch out !"]],
    eval_info: [:yellow, :bright]
  ],
  history_size: 40,
  # "[#{IO.ANSI.magenta()}#{timestamp.()}#{IO.ANSI.reset()} :: " <>
  # ⟖ ⭃ ⧐ ⭄  ⊳   ▷
  default_prompt:
    "#{IO.ANSI.green()}%prefix#{IO.ANSI.reset()} " <>
      "#{IO.ANSI.cyan()}: %counter#{IO.ANSI.green()}  #{IO.ANSI.reset()}",
  history_size: 50,
  inspect: [
    pretty: true,
    limit: :infinity,
    width: 80
  ]
)
