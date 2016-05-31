package analyzer;

public class AnnotationsTest {

	// Foo got a rank of 1. 
	// Only @Test methods are taken into consideration
	
	@Test
	public void foo1() {
		Foo foo = new Foo();
	}
	
	public void foo2() {
		Foo foo = new Foo();
	}
}
