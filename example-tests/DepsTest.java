package analyzer;

public class DepsTest {

	@Test
	public void test() {
		Foo foo = new Foo(4,5, new Bar("Test"));
		assertTrue(Foo.test(new Baax()) && foo.something((Bang) foo.bla(15L), Another.test(new Quux())));
	}
}
