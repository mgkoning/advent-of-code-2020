using System.Linq;
using System.Collections.Generic;

namespace AdventOfCode2020.Shared {
  public static class IntEnumerableExtensions {
    public static int Product(this IEnumerable<int> values) =>
      values.Aggregate(1, (a, b) => a * b);
  }
}