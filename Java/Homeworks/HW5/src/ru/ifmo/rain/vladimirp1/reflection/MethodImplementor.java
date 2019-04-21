package ru.ifmo.rain.vladimirp1.reflection;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Class to generate dummy implementations for methods of interfaces, via reflection
 */
class MethodImplementor {
    /**
     * Does nothing and is private. The class is not intended for instination.
     */
    private MethodImplementor() {}

    /**
     * Gets modifiers for method
     * @param method target method
     * @return method's modifiers
     */
    private static String getModifiers(Method method) {
        return Modifier.toString(method.getModifiers() & ~Modifier.INTERFACE & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT);
    }

    /**
     * Gets method's return type
     * @param method target method
     * @return method's return type as String
     */
    private static String getReturnType(Method method) {
        return method.getReturnType().getCanonicalName();
    }

    /**
     * Gets method's parameters, as string
     * @param method target method
     * @return method's parameters
     */
    private static String getParams(Method method) {
        Class<?>[] types = method.getParameterTypes();
        return IntStream
                .range(0, types.length)
                .mapToObj(x -> types[x].getCanonicalName() + " p" + Integer.toString(x))
                .collect(Collectors.joining(", "));
    }

    /**
     * Gets a dummy return value for method.
     * @param method target method
     * @return some valid return value for method as String
     */
    private static String getReturnValue(Method method) {
        Class<?> c = method.getReturnType();
        if (c.equals(boolean.class)) {
            return "false";
        } else if (c.equals(void.class)) {
            return "";
        } else if (c.isPrimitive()) {
            return "0";
        }

        return "null";
    }

    /**
     * Enumerates all exceptions, which can be thrown from a method in interface.
     * @param method target method
     * @return String, method's throws
     */
    private static String getThrows(Method method) {
        String result = Arrays.stream(method.getExceptionTypes()).map(Class::getCanonicalName).collect(Collectors.joining(","));
        return result.equals("") ? "" : "throws " + result;
    }

    /**
     * Generates a valid signaturem which can be used to implement a method and append it to a {@link StringBuilder}
     * @param builder target StringBuilder
     * @param method target method
     */
    private static void implementSignature(StringBuilder builder, Method method) {
        builder.append(getModifiers(method));
        builder.append(" ");
        builder.append(getReturnType(method));
        builder.append(" ");
        builder.append(method.getName());
        builder.append("(");
        builder.append(getParams(method));
        builder.append(") ");
        builder.append(getThrows(method));
    }

    /**
     * Generates a valid implementation for an abstract method in interface. and append it to a {@link StringBuilder}
     * @param builder target StringBuilder
     * @param method target method
     */
    private static void implementBody(StringBuilder builder, Method method) {
        builder.append("{");
        builder.append("return ");
        builder.append(getReturnValue(method));
        builder.append(";}");
    }

    /**
     * Generates code, which is a syntactically valid method implementation.
     * @param method target method
     * @return method's implementation
     */
    public static String implement(Method method) {
        StringBuilder builder = new StringBuilder();

        implementSignature(builder, method);

        implementBody(builder, method);

        return builder.toString();
    }

}
