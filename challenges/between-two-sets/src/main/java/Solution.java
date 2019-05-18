import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import com.google.common.collect.Multisets;
import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Solution {

  public static void main(String[] args) {
    // int[] a = {3, 4};
    // int[] b = {144, 36, 48};
    int[] a = {2, 4};
    int[] b = {16, 32, 96};
    System.out.format("getTotalX: %d%n", getTotalX(a, b));
  }

  static int getTotalX(int[] a, int[] b) {
    if (a.length == 0 || b.length == 0) {
      return 0;
    }

    Multiset<Integer> allSourceFactors = factorsUnion(Arrays.stream(a));
    // Multiset<Integer> commonDestinationFactors = factorsIntersection(b);
    int smallestCommonFactor = product(allSourceFactors.stream());

    if (Arrays.stream(b).anyMatch(x -> !isFactorOf(smallestCommonFactor, x))) {
      System.out.format("The smallest factor (%d) is not a factor of one destination");
      return 0;
    }

    IntStream destinationsDivided = Arrays.stream(b).map(x -> x / smallestCommonFactor);
    Multiset<Integer> commonDestinationFactors = factorsIntersection(destinationsDivided);
    System.out.print("commonDestinationFactors:");
    commonDestinationFactors
        .stream()
        .forEach(
            x -> {
              System.out.format(" %d", x);
            });
    System.out.println();

    int smallestCommonFactorMultiplier = product(commonDestinationFactors.stream());

    System.out.format("smallestCommonFactorMultiplier: %d%n", smallestCommonFactorMultiplier);
    return smallestCommonFactorMultiplier;
  }

  static Multiset<Integer> factors(int number) {
    System.out.format("factors(%d)%n", number);
    if (number < 1) {
      throw new IllegalArgumentException("must be a positive number");
    } else if (number == 1) {
      System.out.format("No factors for 1%n");
      return HashMultiset.create();
    }
    Multiset<Integer> factors = HashMultiset.create();
    for (int factor = 2; number > 1; factor++) {
      for (; number % factor == 0; number /= factor) {
        System.out.format("\t%d%n", factor);
        factors.add(factor);
      }
    }
    return factors;
  }

  static Multiset<Integer> factorsUnion(IntStream numbers) {
    Stream<Multiset<Integer>> factorsSets = numbers.mapToObj(number -> factors(number));
    return factorsSets.reduce(HashMultiset.create(), (acc, set) -> Multisets.union(acc, set));
  }

  static Multiset<Integer> factorsIntersection(IntStream numbers) {
    System.out.println("factorsIntersection");
    Stream<Multiset<Integer>> factorsSets = numbers.mapToObj(number -> factors(number));
    return factorsSets
        .reduce(
            (acc, set) -> {
              System.out.print("acc: ");
              acc.stream().forEach(x -> System.out.format(" %d", x));
              System.out.println();
              System.out.print("set: ");
              set.stream().forEach(x -> System.out.format(" %d", x));
              System.out.println();
              return Multisets.intersection(acc, set);
            })
        .orElse(HashMultiset.create());
  }

  static int product(Stream<Integer> stream) {
    return stream.reduce(1, (acc, x) -> acc * x);
  }

  static boolean isFactorOf(int factor, int number) {
    return number % factor == 0;
  }
}
