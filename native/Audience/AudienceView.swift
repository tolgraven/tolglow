import SwiftUI
#if APPCLIP
import StoreKit
#endif

struct AudienceView: View {
    @ObservedObject var session: AudienceSession
    @ObservedObject private var client: AudienceClient
    @Environment(\.scenePhase) private var phase
    @Environment(\.colorScheme) private var scheme
    @Environment(\.horizontalSizeClass) private var width
    @Environment(\.dynamicTypeSize) private var typeSize
    @State private var attemptedJoin = false
    #if APPCLIP
    @State private var showFullApp = false
    #endif
    let room: String?

    init(session: AudienceSession, room: String? = nil) {
        self.session = session
        self.client = session.client
        self.room = room
    }

    private var background: Color {
        scheme == .dark ? Color(red: 0.068, green: 0.082, blue: 0.071) : Color(red: 0.95, green: 0.96, blue: 0.94)
    }
    private var surface: Color {
        scheme == .dark ? Color(red: 0.111, green: 0.129, blue: 0.111) : .white
    }
    private var accent: Color {
        scheme == .dark ? Color(red: 0.76, green: 0.84, blue: 0.69) : Color(red: 0.22, green: 0.36, blue: 0.20)
    }
    private var onAccent: Color { scheme == .dark ? Color(red: 0.08, green: 0.12, blue: 0.07) : .white }
    private var wide: Bool { width == .regular && !typeSize.isAccessibilitySize }

    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 20) {
                    Text("Dance floor")
                        .font(.largeTitle.bold())
                        .accessibilityAddTraits(.isHeader)
                    if client.joined {
                        joinedControls
                    } else if client.looking || client.connecting {
                        discovery
                    } else {
                        if attemptedJoin { Text(client.status).font(.headline) }
                        introduction
                    }
                    #if APPCLIP
                    Button { showFullApp = true } label: {
                        Label("Use phone or Watch motion in Tolglow", systemImage: "arrow.up.forward.app")
                            .font(.subheadline.weight(.medium))
                            .frame(minHeight: 44, alignment: .leading)
                    }
                    .appStoreOverlay(isPresented: $showFullApp) { SKOverlay.AppClipConfiguration(position: .bottom) }
                    #endif
                }
                .frame(maxWidth: 700, alignment: .leading)
                .padding(.horizontal, wide ? 32 : 20)
                .padding(.vertical, 24)
                .frame(maxWidth: .infinity)
            }
            .background(background)
            .safeAreaInset(edge: .bottom, spacing: 0) { actionBar }
            .toolbar(.hidden, for: .navigationBar)
        }
        .tint(accent)
        .onChange(of: client.joined) { _, joined in if joined { session.startInputs() } }
        .onChange(of: session.input) { _, _ in session.resetInput() }
        .onChange(of: room) { _, _ in client.leave() }
        .onChange(of: phase) { _, value in
            if value == .background { session.enterBackground() }
            if value == .active { session.foreground() }
        }
        .onDisappear { if phase == .active { client.leave() } }
    }

    private var introduction: some View {
        let layout = wide
            ? AnyLayout(HStackLayout(alignment: .top, spacing: 26))
            : AnyLayout(VStackLayout(alignment: .leading, spacing: 18))
        return layout {
            Text("Send your steps and movement intensity to the stage lights over Bluetooth.")
                .font(.title3).fixedSize(horizontal: false, vertical: true)
                .frame(maxWidth: wide ? 235 : .infinity, alignment: .leading)
            VStack(alignment: .leading, spacing: 0) {
                ForEach(Array(AudienceInput.allCases.enumerated()), id: \.element) { index, input in
                    if index > 0 { Divider() }
                    HStack(alignment: .top, spacing: 12) {
                        Image(systemName: icon(input))
                            .font(.title3).foregroundStyle(accent).frame(width: 30).accessibilityHidden(true)
                        VStack(alignment: .leading, spacing: 3) {
                            Text(input.rawValue).font(.headline)
                            Text(detail(input)).font(.subheadline).foregroundStyle(.secondary)
                        }
                        .fixedSize(horizontal: false, vertical: true)
                    }
                    .padding(.vertical, 13)
                    .accessibilityElement(children: .combine)
                }
            }
            .padding(.horizontal, 16).background(surface, in: RoundedRectangle(cornerRadius: 14))
        }
    }

    private func icon(_ input: AudienceInput) -> String {
        switch input {
        case .touch: return "hand.tap"
        case .camera: return "camera"
        #if !APPCLIP
        case .motion: return "gyroscope"
        #endif
        }
    }

    private func detail(_ input: AudienceInput) -> String {
        switch input {
        case .touch: return "Send one step with each tap."
        case .camera: return "Estimate steps from camera images that stay on this device."
        #if !APPCLIP
        case .motion: return "Measure movement with your phone. Sharing individual motion axes is optional."
        #endif
        }
    }

    private var discovery: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack(spacing: 10) {
                ProgressView().tint(accent)
                Text(client.status).font(.headline)
            }
            ForEach(client.venues) { venue in
                Button { client.join(venue.id) } label: {
                    HStack {
                        Label(venue.name, systemImage: "dot.radiowaves.left.and.right")
                        Spacer()
                        Image(systemName: "chevron.right").accessibilityHidden(true)
                    }
                    .padding(.horizontal, 14).frame(minHeight: 52).background(surface, in: RoundedRectangle(cornerRadius: 10))
                }
                .disabled(client.connecting)
            }
        }
    }

    private var joinedControls: some View {
        VStack(alignment: .leading, spacing: 16) {
            Label(client.status, systemImage: "checkmark.circle.fill")
                .font(.subheadline.weight(.semibold)).foregroundStyle(accent)
            if wide {
                HStack(alignment: .top, spacing: 16) {
                    meters.frame(maxWidth: .infinity)
                    inputControls.frame(maxWidth: .infinity)
                }
            } else {
                meters
                inputControls
            }
            #if !APPCLIP
            VStack(alignment: .leading, spacing: 8) {
                Text("Optional sharing").font(.headline)
                if session.input == .motion {
                    Toggle("Phone acceleration and rotation", isOn: $session.shareMotionAxes).frame(minHeight: 48)
                }
                Toggle("Watch input while iPhone is locked", isOn: $session.allowLockedWatch).frame(minHeight: 48)
                Text("Choose whether to share pulse in the Watch app.")
                    .font(.subheadline).foregroundStyle(.secondary)
            }
            .padding(16).background(surface, in: RoundedRectangle(cornerRadius: 14))
            #endif
            if !session.message.isEmpty { Text(session.message).font(.subheadline).foregroundStyle(.secondary) }
        }
    }

    private var meters: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack(alignment: .top, spacing: 20) {
                metric("Dance intensity", session.effort.formatted(.percent.precision(.fractionLength(0))))
                metric("Steps", session.totalSteps.formatted())
            }
            ProgressView(value: session.effort).tint(accent).accessibilityHidden(true)
        }
        .padding(16).frame(maxWidth: .infinity, alignment: .leading)
        .background(surface, in: RoundedRectangle(cornerRadius: 14))
    }

    private func metric(_ name: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(name).font(.subheadline).foregroundStyle(.secondary)
            Text(value).font(.title2.weight(.semibold)).monospacedDigit()
        }.accessibilityElement(children: .combine)
    }

    private var inputControls: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("Movement input").font(.headline)
            if typeSize.isAccessibilitySize {
                Picker("Movement input", selection: $session.input) { inputOptions }.frame(minHeight: 48)
            } else {
                HStack(spacing: 4) {
                    ForEach(AudienceInput.allCases) { input in
                        let selected = session.input == input
                        Button { session.input = input } label: {
                            Text(input.rawValue).font(.subheadline.weight(.semibold))
                                .foregroundStyle(selected ? onAccent : .primary)
                                .padding(.horizontal, 4).frame(maxWidth: .infinity, minHeight: 48)
                                .background(selected ? accent : background, in: RoundedRectangle(cornerRadius: 8))
                        }
                        .buttonStyle(.plain)
                        .accessibilityAddTraits(selected ? .isSelected : [])
                    }
                }
            }
            Text(session.input == .camera ? "Prop up the phone where the front camera can see your knees and feet. Images stay on this device." : detail(session.input))
                .font(.subheadline).foregroundStyle(.secondary)
        }
        .padding(16).frame(maxWidth: .infinity, alignment: .leading)
        .background(surface, in: RoundedRectangle(cornerRadius: 14))
    }

    private var inputOptions: some View {
        ForEach(AudienceInput.allCases) { input in Text(input.rawValue).tag(input) }
    }

    @ViewBuilder private var dockControls: some View {
        if client.joined {
            if session.input == .touch {
                primary("Tap a step", symbol: "hand.tap.fill") { session.tap() }
            }
            Button(role: .destructive) { client.leave() } label: {
                Text("Leave stage").frame(maxWidth: .infinity, minHeight: 46).contentShape(Rectangle())
            }
        } else if client.looking || client.connecting {
            Button { client.leave() } label: {
                Text("Cancel").frame(maxWidth: .infinity, minHeight: 48).contentShape(Rectangle())
            }.buttonStyle(.bordered)
        } else {
            primary("Find a stage", symbol: "dot.radiowaves.left.and.right") {
                attemptedJoin = true
                client.discover(room: room)
            }
        }
    }

    private func primary(_ title: String, symbol: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Label(title, systemImage: symbol)
                .font(.headline).foregroundStyle(onAccent)
                .padding(.horizontal, 12).frame(maxWidth: .infinity, minHeight: 52)
                .background(accent, in: RoundedRectangle(cornerRadius: 12))
        }.buttonStyle(.plain)
    }

    private var actionBar: some View {
        VStack(spacing: 0) {
            Divider()
            VStack(spacing: 0) { dockControls }
                .frame(maxWidth: 700).padding(.horizontal, wide ? 32 : 20).padding(.vertical, 8)
        }
        .frame(maxWidth: .infinity).background(background)
    }
}
