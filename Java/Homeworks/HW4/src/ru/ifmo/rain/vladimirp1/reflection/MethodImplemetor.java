package ru.ifmo.rain.vladimirp1.reflection;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


public class MethodImplemetor {
    private static String getModifiers(Method method) {
        return Modifier.toString(method.getModifiers() & ~Modifier.INTERFACE & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT);
    }

    private static String getReturnType(Method method) {
        return method.getReturnType().getCanonicalName();
    }

    private static String getParams(Method method) {
        Class<?>[] types = method.getParameterTypes();
        return IntStream
                .range(0, types.length)
                .mapToObj(x -> types[x].getCanonicalName() + " p" + Integer.toString(x))
                .collect(Collectors.joining(", "));
    }

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


    private static String getThrows(Method method) {
        String result = Arrays.stream(method.getExceptionTypes()).map(Class::getCanonicalName).collect(Collectors.joining(","));
        return result.equals("") ? "" : "throws " + result;
    }

    public static void implementStatic(StringBuilder builder) {

    }

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

    public static void implementBody(StringBuilder builder, Method method) {
        builder.append("{");
        builder.append("return ");
        builder.append(getReturnValue(method));
        builder.append(";}");
    }

    public static String implement(Method method) {
        StringBuilder builder = new StringBuilder();

        implementSignature(builder, method);

        implementBody(builder, method);

        return builder.toString();
    }

}
