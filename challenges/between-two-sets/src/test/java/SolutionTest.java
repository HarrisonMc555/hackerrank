import static org.junit.Assert.*;

import org.junit.Test;

public class SolutionTest {
  @Test
  public void test0() {
    int[] a = {2, 4};
    int[] b = {16, 32, 96};
    assertEquals(3, Solution.getTotalX(a, b));
  }

  @Test
  public void test1() {
    int[] a = {3, 4};
    int[] b = {24, 48};
    assertEquals(2, Solution.getTotalX(a, b));
  }

  @Test
  public void test3() {
    int[] a = {3, 9, 6};
    int[] b = {36, 72};
    assertEquals(2, Solution.getTotalX(a, b));
  }
}
