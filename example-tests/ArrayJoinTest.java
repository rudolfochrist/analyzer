package analyzer;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.hamcrest.CoreMatchers;
import org.junit.Test;

// rank Joiner 1: because of string similarity
public class ArrayJoinTest {
	
	private static final String[] array = new String[] {"a", "b", "c"};

	// rank Toiner 1: object creation
	private static final Toiner t = new Toiner();
	private static Toiner t2;

	@Test
	public void test_simple_join() throws Exception {
        // rank Joiner 2: object creation
		Joiner j = new Joiner();

        // rank Joiner 3: method call in assert*
		assertThat(j.join(array), equalTo("abc"));
	}

	@Test
	public void test_join_with_comma() throws Exception {
        // rank Joiner 4: object creatiopn
		Joiner j = new Joiner(",");
        // rank Joiner 5: method call in assert*
		assertThat(j.join(array), equalTo("a,b,c"));
	}

	@Test
	public void not_interesting() throws Exception {
		Joiner[] js = new Joiner[1];
        // doesn't get ranked because object not assigned to variable
		new Toiner();
        // rank Toiner 2: object creation
		Toiner tt = new Toiner();
        // rank Toiner 3: assignment
		t2 = new Toiner();
        // rank Toiner 4: method call in assert*
		assertTrue(tt.getValue());
        // rank Toiner 5: method call on tt
        // rank Toiner 6: method call on t2
		assertFalse(tt.getUID().equals(t2.getUID()));
	}

	@Test
	public void identity_test() throws Exception {
        // rank Joiner 6: object creation
		Joiner a = new Joiner();
		Joiner b = a;
        // rank Joiner 8: usage in assert*
		assertTrue(a == b);
        // rank Joiner 10: ussage in assert*
		assertFalse(a != b);
	}

	@Test
	public void in_assert_creation() throws Exception {
        // rank Joiner 11: object creation in  assert*
        // we don't rank the object creation individually because this is one off.
        assertEquals("abc", new Joiner().join(array));
        // rank Joiner 12: object creation in assert*
		assertThat(new Joiner(), notNullValue());
	}
}
