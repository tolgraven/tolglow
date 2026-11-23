package org.tolglow.embedded;

import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;

/** Native Image API used by the owning iOS app. All paths come from its sandbox or a folder picker. */
public final class StudioRuntime {
    public static void main(String[] args) throws Exception {
        if (args.length > 1) throw new IllegalArgumentException("Pass at most one show folder.");
        int port = tolglow.fixed_host.open(System.getProperty("user.home"), args.length == 0 ? "" : args[0]);
        try {
            System.out.println("Tolglow Studio listening on http://127.0.0.1:" + port);
            while (System.in.read() != -1) { /* The launching process owns this pipe. */ }
        } finally {
            tolglow.fixed_host.close();
        }
    }

    @CEntryPoint(name = "tolglow_open_local")
    public static int open(IsolateThread thread, CCharPointer home, CCharPointer folder) {
        try {
            return tolglow.fixed_host.open(CTypeConversion.toJavaString(home), CTypeConversion.toJavaString(folder));
        } catch (Throwable error) {
            error.printStackTrace();
            return -1;
        }
    }

    @CEntryPoint(name = "tolglow_close_local")
    public static int close(IsolateThread thread) {
        try {
            tolglow.fixed_host.close();
            return 0;
        } catch (Throwable error) {
            error.printStackTrace();
            return -1;
        }
    }
}
