package foo;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.*;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

public class FoobarTest {

	@Test
	public void test() {
		String abbrev = StringUtils.abbreviate("FoobarFoobarFoobar", 6);
		assertThat(abbrev, equalTo(StringUtils.abbreviate("FoobarFoobarFoobar", 6)));
	}

}
