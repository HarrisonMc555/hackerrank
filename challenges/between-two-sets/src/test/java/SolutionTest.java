import static org.junit.Assert.*;

import org.junit.Test;

public class SolutionTest {
  @Test
  public void testSomeSolutionMethod() {
    int[] a = {2, 4};
    int[] b = {16, 32, 96};
    assertEquals(3, Solution.getTotalX(a, b));
  }
}
