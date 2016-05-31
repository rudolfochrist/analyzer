package analyzer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.hamcrest.CoreMatchers;
import org.junit.Test;

public class ArrayJoinTest {
	
	private static final String[] array = new String[] {"a", "b", "c"};
	private static final Toiner t = new Toiner();
	private static Toiner t2;

	@Test
	public void test_simple_join() throws Exception {
		Joiner j = new Joiner();
	
		assertThat(j.join(array), equalTo("abc"));
	}
	
	@Test
	public void test_join_with_comma() throws Exception {
		Joiner j = new Joiner(",");
		assertThat(j.join(array), equalTo("a,b,c"));
	}
	
	@Test
	public void not_interesting() throws Exception {
		Joiner[] js = new Joiner[1];
		new Toiner();
		Toiner tt = new Toiner();
		t2 = new Toiner();
		assertTrue(tt.getValue());
		assertFalse(tt.getUID().equals(t2.getUID()));
	}
	
	@Test
	public void identity_test() throws Exception {
		Joiner a = new Joiner();
		Joiner b = a;
		assertTrue(a == b);
		assertFalse(a != b);
	}
	
	@Test
	public void in_assert_creation() throws Exception {
		assertEquals("abc", new Joiner().join(array));
		assertThat(new Joiner(), notNullValue());
	}
}
