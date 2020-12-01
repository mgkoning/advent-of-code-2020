using System;
using System.IO;

namespace AdventOfCode2020.Shared {
  public static class Input {
    public static DirectoryInfo InputsDirectory { get; } = FindInputsDirectory() ??
      throw new DirectoryNotFoundException("inputs directory could not be determined");

    private static DirectoryInfo? FindInputsDirectory() {
      DirectoryInfo? find(DirectoryInfo? start) {
        if (start == null) { return null; }
        var maybeInput = new DirectoryInfo(Path.Combine(start.FullName, "input"));
        return maybeInput.Exists ? maybeInput : find(start.Parent);
      }

      return find(new DirectoryInfo("."));
    }
  }
}
