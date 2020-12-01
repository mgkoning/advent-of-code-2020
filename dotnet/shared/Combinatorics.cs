using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2020.Shared {
  public static class Combinatorics {

    public static IEnumerable<T> Infinite<T>(T value) {
      while(true) {
        yield return value;
      }
    }

    public static IEnumerable<IEnumerable<T>> Combinations<T>(int number, IEnumerable<T> values) {
      if (number == 1) {
        return values.Select(v => new [] { v });
      }
      if (!values.Any()) { return Enumerable.Empty<IEnumerable<T>>(); }
      var (head, tail) = (values.First(), values.Skip(1));
      return Infinite(head)
        .Zip(Combinations(number - 1, tail), (h, t) => new [] { h }.Concat(t))
        .Concat(Combinations(number, tail));
    }
  }
}