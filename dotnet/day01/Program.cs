﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode2020.Shared;

namespace AdventOfCode2020.Day01 {
  class Program {

    
    static void Main() {
      var input = File
        .ReadAllLines(Path.Combine(Input.InputsDirectory.FullName, "day01.txt"))
        .Select(x => int.Parse(x))
        .ToList();
      Part1(input);
      Part2(input);
      Console.WriteLine("Part 1 (alt):");
      Console.WriteLine(SolutionAlt(2020, 2, input));
      Console.WriteLine("Part 2 (alt):");
      Console.WriteLine(SolutionAlt(2020, 3, input));
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

    private static int SolutionAlt(int target, int numValues, IList<int> input) =>
      Enumerable.First(
        from combination in Combinatorics.Combinations(numValues, input)
        where combination.Sum() == target
        select combination.Product()
      );

    private static void Part2(IList<int> input) {
      Console.WriteLine("Part 2:");
      for(int i = 0; i < input.Count-2; i++) {
        for (int j = i+1; j < input.Count-1; j++) {
          for (int k = j+1; k < input.Count; k++) {
            var (a, b, c) = (input[i], input[j], input[k]);
            if (a + b + c == 2020) {
              System.Console.WriteLine(a * b * c);
              return;
            }
          }
        }
      }
      System.Console.WriteLine("Nothing found?");
    }
  }
}
