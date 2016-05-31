
import org.junit.Test;

public class LoginTest {
	
	private static final String username = "spc";
	private static final String password = "foobar";

	@Test
	public void test() {
		Session session = Login.login(username, password);
		assertTrue(session.isLoggedIn());
	}

}
