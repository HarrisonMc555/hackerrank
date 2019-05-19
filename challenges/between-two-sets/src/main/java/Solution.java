import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Solution {

  public static void main(String[] args) {
    int[] a = {3, 9, 6};
    int[] b = {36, 72};
    System.out.println(Solution.getTotalX(a, b));
  }

  static int getTotalX(int[] a, int[] b) {
    if (a.length == 0 || b.length == 0) {
      return 0;
    }

    Map<Integer, Integer> allSourceFactors = factorsUnion(Arrays.stream(a));
    // System.out.println("allSourceFactors:");
    // printMap(allSourceFactors);
    int smallestCommonFactor =
        allSourceFactors
            .entrySet()
            .stream()
            .reduce(
                1, (acc, entry) -> acc * pow(entry.getKey(), entry.getValue()), (x, y) -> x * y);

    // System.out.format("smallestCommonFactor: %d%n", smallestCommonFactor);

    if (Arrays.stream(b).anyMatch(x -> !isFactorOf(smallestCommonFactor, x))) {
      return 0;
    }

    IntStream destinationsDivided = Arrays.stream(b).map(x -> x / smallestCommonFactor);
    Map<Integer, Integer> commonDestinationFactors = factorsIntersection(destinationsDivided);

    Stream<Integer> counts =
        commonDestinationFactors.entrySet().stream().map(entry -> entry.getValue());
    Stream<Integer> countsPlusOne = counts.map(x -> x + 1);
    int numberOfCombinations = product(countsPlusOne);

    return numberOfCombinations;
  }

  static Map<Integer, Integer> factors(int number) {
    if (number < 1) {
      throw new IllegalArgumentException("must be a positive number");
    } else if (number == 1) {
      return new HashMap<Integer, Integer>();
    }
    Map<Integer, Integer> factors = new HashMap<Integer, Integer>();
    for (int factor = 2; number > 1; factor++) {
      for (; number % factor == 0; number /= factor) {
        incrementValue(factors, factor);
      }
    }
    return factors;
  }

  static Map<Integer, Integer> factorsUnion(IntStream numbers) {
    Stream<Map<Integer, Integer>> factorsSets = numbers.mapToObj(number -> factors(number));
    return factorsSets.reduce(new HashMap<Integer, Integer>(), (acc, set) -> union(acc, set));
  }

  static Map<Integer, Integer> factorsIntersection(IntStream numbers) {
    Stream<Map<Integer, Integer>> factorsSets = numbers.mapToObj(number -> factors(number));
    return factorsSets
        .reduce((acc, set) -> intersection(acc, set))
        .orElse(new HashMap<Integer, Integer>());
  }

  static int product(Stream<Integer> stream) {
    return stream.reduce(1, (acc, x) -> acc * x);
  }

  static boolean isFactorOf(int factor, int number) {
    return number % factor == 0;
  }

  static Map<Integer, Integer> intersection(
      Map<Integer, Integer> set1, Map<Integer, Integer> set2) {
    Map<Integer, Integer> result = new HashMap<Integer, Integer>();
    for (Map.Entry<Integer, Integer> entry : set1.entrySet()) {
      int key = entry.getKey();
      int value1 = entry.getValue();
      int value2 = set2.getOrDefault(key, 0);
      int smallestValue = Math.min(value1, value2);
      if (smallestValue > 0) {
        result.put(key, smallestValue);
      }
    }
    return result;
  }

  static Map<Integer, Integer> union(Map<Integer, Integer> set1, Map<Integer, Integer> set2) {
    // System.out.println("== union begin ==");
    // System.out.println("set1 (before):");
    // printMap(set1);
    // System.out.println("set2 (before):");
    // printMap(set2);

    // System.out.println("doing union:");
    Map<Integer, Integer> result = new HashMap<Integer, Integer>();
    for (Map.Entry<Integer, Integer> entry : set1.entrySet()) {
      int key = entry.getKey();
      int value1 = entry.getValue();
      int value2 = set2.getOrDefault(key, 0);
      int largestValue = Math.max(value1, value2);
      // System.out.format(
      //     "key: %d%nvalue1: %d%nvalue2: %d%nlargestValue: %d%n", key, value1, value2,
      // largestValue);
      result.put(key, largestValue);
    }
    for (Map.Entry<Integer, Integer> entry : set2.entrySet()) {
      int key = entry.getKey();
      int value1 = entry.getValue();
      int value2 = set1.getOrDefault(key, 0);
      int largestValue = Math.max(value1, value2);
      // System.out.format(
      //     "key: %d%nvalue1: %d%nvalue2: %d%nlargestValue: %d%n", key, value1, value2,
      // largestValue);
      result.put(key, largestValue);
    }
    // System.out.println("result (after):");
    // printMap(result);
    // System.out.println("== union end ==");
    return result;
  }

  static void incrementValue(Map<Integer, Integer> set, int key) {
    int oldCount = set.getOrDefault(key, 0);
    int newCount = oldCount + 1;
    set.put(key, newCount);
  }

  static int pow(int base, int exponent) {
    int result;
    for (result = 1; exponent >= 1; exponent--) {
      result *= base;
    }
    return result;
  }

  // static void printMap(Map<Integer, Integer> map) {
  //   for (Map.Entry<Integer, Integer> entry : map.entrySet()) {
  //     System.out.format("%d: %d%n", entry.getKey(), entry.getValue());
  //   }
  // }
}
