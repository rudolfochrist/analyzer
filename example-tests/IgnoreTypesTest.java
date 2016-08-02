package analyzer;

public class IgnoreTypesTest {

	@Test
	public void test() {
		// Ranking ignores types found in java.lang and java.util
        // as well as org.junit and org.hamcrest

        String s = new String(); // doesn't rank
        List<String> l = new ArrayList<String>();  //doesn't rank
        Foo foo = new Foo(); // gets ranked

        Assert.assertEquals(s, "abc");  // no ranking on Assert or String s
        Assert.assertTrue(foo.isBest()); // no ranking on Assert but foo gets ranked.
	}

}
