import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import com.google.common.collect.Multisets;
import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Solution {

  static int getTotalX(int[] a, int[] b) {
    if (a.length == 0 || b.length == 0) {
      return 0;
    }

    Stream<Multiset<Integer>> sourceFactorSets = Arrays.stream(a).mapToObj(x -> getFactors(x));

    Multiset<Integer> sourceAllFactors =
        sourceFactorSets.reduce(
            HashMultiset.create(),
            (factorsSoFar, factors) -> Multisets.union(factorsSoFar, factors));
    int smallestCommonFactor = sourceAllFactors.stream().reduce(1, (acc, x) -> acc * x);

    if (Arrays.stream(b).anyMatch(x -> x % smallestCommonFactor != 0)) {
      return 0;
    }

    IntStream destinationsDivided = Arrays.stream(b).map(x -> x / smallestCommonFactor);
    Stream<Multiset<Integer>> destinationFactorSets =
        destinationsDivided.mapToObj(x -> getFactors(x));
    Multiset<Integer> destinationCommonFactors =
        destinationFactorSets.reduce(
            HashMultiset.create(),
            (factorsSoFar, factors) -> Multisets.intersection(factorsSoFar, factors));
    int smallestCommonFactorMultiplier =
        destinationCommonFactors.stream().reduce(1, (acc, x) -> acc * x);

    // if (true) return smallestCommonFactorMultiplier;

    return smallestCommonFactorMultiplier;
  }

  static Multiset<Integer> getFactors(int number) {
    if (number < 1) {
      throw new IllegalArgumentException("must be a positive number");
    } else if (number == 1) {
      return HashMultiset.create();
    }
    System.out.println(number);
    Multiset<Integer> factors = HashMultiset.create();
    int factor = 2;
    while (number > 1) {
      while (number % factor == 0) {
        factors.add(factor);
        number /= factor;
      }
      factor++;
    }
    return factors;
  }
}
