import SwiftUI

struct AudienceView: View {
    @ObservedObject var session: AudienceSession
    @ObservedObject private var client: AudienceClient
    @Environment(\.scenePhase) private var phase
    let room: String?

    init(session: AudienceSession, room: String? = nil) {
        self.session = session
        self.client = session.client
        self.room = room
    }
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 18) {
                    Text(client.status).font(.headline).accessibilityAddTraits(.updatesFrequently)
                    if client.joined {
                        Picker("Movement input", selection: $session.input) {
                            ForEach(AudienceInput.allCases) { input in Text(input.rawValue).tag(input) }
                        }.pickerStyle(.segmented)
                        if session.input == .camera {
                            Text("Prop up the phone so the front camera sees your knees and feet. Only movement estimates leave the phone.")
                        }
                        if session.input == .touch {
                            Button("Tap a step") { session.tap() }.buttonStyle(.borderedProminent)
                                .controlSize(.large).frame(maxWidth: .infinity)
                        }
                        LabeledContent("Dance intensity", value: session.effort.formatted(.percent.precision(.fractionLength(0))))
                        ProgressView(value: session.effort)
                        LabeledContent("Steps", value: String(session.totalSteps))
                        if !session.message.isEmpty { Text(session.message).foregroundStyle(.secondary) }
                        #if !APPCLIP
                        if session.input == .motion {
                            Toggle("Share phone acceleration and rotation", isOn: $session.shareMotionAxes)
                        }
                        Toggle("Keep sharing Watch input when iPhone locks", isOn: $session.allowLockedWatch)
                        #endif
                        Button("Leave stage", role: .destructive) { client.leave() }
                    } else {
                        Text("Join a nearby stage over Bluetooth. Your activity contributes to the lighting mix.").foregroundStyle(.secondary)
                        if client.looking {
                            ForEach(client.venues) { venue in
                                Button { client.join(venue.id) } label: {
                                    Label(venue.name, systemImage: "dot.radiowaves.left.and.right").frame(maxWidth: .infinity, alignment: .leading)
                                }.buttonStyle(.bordered).controlSize(.large)
                            }
                        }
                        if client.looking || client.connecting {
                            Button("Cancel") { client.leave() }
                        } else {
                            Button("Find nearby stages") { client.discover(room: room) }.buttonStyle(.borderedProminent)
                        }
                    }
                }.padding()
            }.navigationTitle("Dance floor")
        }
        .onChange(of: client.joined) { _, joined in if joined { session.startInputs() } }
        .onChange(of: session.input) { _, _ in session.resetInput() }
        .onChange(of: room) { _, _ in client.leave() }
        .onChange(of: phase) { _, value in
            if value == .background { session.enterBackground() }
            if value == .active { session.foreground() }
        }
        .onDisappear { if phase == .active { client.leave() } }
    }
}
