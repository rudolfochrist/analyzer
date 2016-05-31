package analyzer;

public class IgnoreTypesTest {
	
	private String gS = new String();
	private Joiner gJ = new Joiner();
	
	@Test
	public void test() {
		String s = new String(); // should not be in types
		Joiner j = new Joiner(); // should be in types.
	}

}
