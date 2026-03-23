package io.github.kaluchi.jdtbridge;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;

/**
 * Shared JDT utilities used by multiple handlers.
 */
class JdtUtils {

    static IType findType(String fqn) throws JavaModelException {
        var model = JavaCore.create(
                ResourcesPlugin.getWorkspace().getRoot());
        for (IJavaProject project : model.getJavaProjects()) {
            IType type = project.findType(fqn);
            if (type != null && type.exists()) return type;
        }
        return null;
    }

    static IMethod findMethod(IType type, String name, int arity)
            throws JavaModelException {
        for (IMethod m : type.getMethods()) {
            if (m.getElementName().equals(name)) {
                if (arity < 0 || m.getNumberOfParameters() == arity)
                    return m;
            }
        }
        return null;
    }

    static List<IMethod> findMethods(IType type, String name, int arity)
            throws JavaModelException {
        List<IMethod> result = new ArrayList<>();
        for (IMethod m : type.getMethods()) {
            if (m.getElementName().equals(name)) {
                if (arity < 0 || m.getNumberOfParameters() == arity) {
                    result.add(m);
                }
            }
        }
        return result;
    }

    static int parseArity(String s) {
        if (s == null) return -1;
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    static String typeKind(IType type) throws JavaModelException {
        if (type.isAnnotation()) return "annotation";
        if (type.isEnum()) return "enum";
        if (type.isInterface()) return "interface";
        return "class";
    }

    /**
     * Wait for Eclipse auto-build to finish, with a 2-minute
     * safety timeout to prevent indefinite hangs.
     */
    static void joinAutoBuild() throws InterruptedException {
        NullProgressMonitor monitor = new NullProgressMonitor();
        Thread.startVirtualThread(() -> {
            try {
                Thread.sleep(120_000);
            } catch (InterruptedException e) { /* ok */ }
            monitor.setCanceled(true);
        });
        Job.getJobManager().join(
                ResourcesPlugin.FAMILY_AUTO_BUILD, monitor);
    }

    static String compactSignature(IMethod m) throws JavaModelException {
        StringBuilder sig = new StringBuilder();
        sig.append(m.getElementName()).append("(");
        String[] paramTypes = m.getParameterTypes();
        for (int i = 0; i < paramTypes.length; i++) {
            if (i > 0) sig.append(", ");
            sig.append(Signature.toString(paramTypes[i]));
        }
        sig.append(")");
        return sig.toString();
    }
}
