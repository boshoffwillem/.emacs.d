# csharp-testing-mode

This is a minor mode for conveniently running C# unit tests.

## Keybindings

All keybindings in csharp-testing-mode start with `C-c t`
and then a two-letter mnemonic shortcut.

* `td`: toggle-deffered will toggle a test's deferred state by commenting or uncommenting
the `[Fact]` or `[Theory]` attributes.
