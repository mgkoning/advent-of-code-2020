using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day01 {
  class Program {

    private static DirectoryInfo InputsDirectory { get; } = FindInputsDirectory() ??
      throw new DirectoryNotFoundException("inputs directory could not be determined");
    private static DirectoryInfo? FindInputsDirectory() {
      DirectoryInfo? find(DirectoryInfo? start) {
        if (start == null) { return null; }
        var maybeInput = new DirectoryInfo(Path.Combine(start.FullName, "input"));
        return maybeInput.Exists ? maybeInput : find(start.Parent);
      }

      return find(new DirectoryInfo("."));
    }

    static void Main() {
      var input = File
        .ReadAllLines(Path.Combine(InputsDirectory.FullName, "day01.txt"))
        .Select(x => int.Parse(x))
        .ToList();
      Part1(input);
      Part2(input);
    }

    private static void Part1(IList<int> input) {
      var (first, rest) = (input.First(), input.Skip(1));
      var target = 2020 - first;
      if (rest.Any(r => r == target)) {
        Console.WriteLine("Part 1:");
        Console.WriteLine(target * first);
      } else {
        Part1(rest.ToList());
      }
    }

    private static void Part2(IList<int> input) {
      Console.WriteLine("Part 2:");
      for(int i = 0; i < input.Count; i++) {
        for (int j = i+1; j < input.Count; j++) {
          for (int k = j+1; k < input.Count; k++) {
            var (a, b, c) = (input[i], input[j], input[k]);
            if (a + b + c == 2020) {
              System.Console.WriteLine(a * b * c);
            }
          }
        }
      }
    }
  }
}
