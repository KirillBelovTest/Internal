(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/Internal",
    "Description" -> "Internal",
    "Creator" -> "Kirill Belov",
    "License" -> "MIT",
    "PublisherID" -> "KirillBelov",
    "Version" -> "1.0.9",
    "WolframVersion" -> "13+",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"KirillBelov`Internal`", "Internal.wl"},
          {
            "KirillBelov`Internal`Compilation`",
            "Compilation.wl"
          },
          {
            "KirillBelov`Internal`Binary`",
            "Binary.wl"
          },
          {
            "KirillBelov`Internal`Tasks`",
            "Tasks.wl"
          },
          {
            "KirillBelov`Internal`Functions`",
            "Functions.wl"
          },
          {
            "KirillBelov`Internal`Console`",
            "Console.wl"
          },
          {
            "KirillBelov`Internal`Alpha`",
            "Alpha.wl"
          }
        },
        "Symbols" -> {"KirillBelov`Internal`AssocMatchQ"}
      },
      {"Documentation", "Language" -> "English"},
      {"LibraryLink", "Root" -> "LibraryResources"},
      {
        "Asset",
        "Assets" -> {
          {"License", "./LICENSE"},
          {"ReadMe", "./README.md"}
        }
      }
    }
  |>
]
