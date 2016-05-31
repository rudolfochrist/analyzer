package analyzer;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.visitor.GenericVisitor;

public interface AnalyzerVisitor<R, A> extends GenericVisitor<R, A> {

	public R visit(Node node, A arg);
}
