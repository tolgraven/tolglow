import SwiftUI
import TolglowSensorCore

struct DanceAudienceView: View {
    @ObservedObject var workout: DanceWorkout
    @ObservedObject private var link: WatchFloorLink
    @ObservedObject private var stage: AudienceClient
    @State private var sharePulse = false

    init(workout: DanceWorkout) {
        self.workout = workout
        self.link = workout.link
        self.stage = workout.link.stage
    }

    private var participating: Bool { workout.active || workout.requesting }

    private func routeButton(_ route: AudienceWatchRoute, title: String, icon: String, id: String) -> some View {
        Button { link.select(route) } label: {
            Label(title, systemImage: link.route == route ? "checkmark.circle.fill" : icon)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .accessibilityIdentifier(id)
        .accessibilityAddTraits(link.route == route ? .isSelected : [])
        .disabled(participating || !route.available(phoneJoined: link.phoneJoined))
    }

    private var destination: some View {
        VStack(alignment: .leading, spacing: 8) {
            routeButton(.pairedPhone, title: "Paired iPhone", icon: "iphone", id: "route-paired")
            routeButton(.nearbyStage, title: "Nearby stage", icon: "antenna.radiowaves.left.and.right", id: "route-direct")
            if link.route == .nearbyStage {
                if stage.joined {
                    Label("Connected to stage", systemImage: "checkmark.circle")
                    if !participating { Button("Disconnect") { workout.stop() } }
                } else {
                    Text("Watch only. Leave the floor on your iPhone first.").font(.caption)
                    Button(stage.looking ? "Stop searching" : "Find stages") {
                        if stage.looking { workout.stop("Stage search stopped.") } else { link.discover() }
                    }.disabled(stage.connecting)
                    ForEach(stage.venues) { venue in
                        Button(venue.name) { stage.join(venue.id) }
                    }
                    Text(stage.status).font(.caption2)
                }
            } else if !participating {
                Text(link.phoneJoined ? "Your iPhone is on the floor. Leave on iPhone to join directly." :
                     "Your Watch and iPhone share one place on the floor.").font(.caption)
            }
        }
    }

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 10) {
                destination
                if workout.active {
                    Gauge(value: workout.effort) { Text("Dance effort") }
                    Text("\(workout.steps) motion steps").monospacedDigit()
                    if let bpm = workout.bpm {
                        Label("\(bpm) BPM", systemImage: "heart.fill").monospacedDigit()
                    } else if sharePulse {
                        Text("No recent heart rate").font(.caption)
                    }
                } else {
                    Toggle("Share heart rate", isOn: $sharePulse).disabled(workout.requesting)
                    if sharePulse {
                        Text("Start a dance workout to share your latest BPM with the floor. Tolglow discards the workout when you leave.")
                            .font(.caption)
                    }
                }
                if workout.active || workout.requesting {
                    Button(workout.active ? "Leave floor" : "Cancel", role: .destructive) { workout.stop() }
                } else {
                    Button(sharePulse ? "Start dance workout" : "Join with motion") { workout.start(withPulse: sharePulse) }
                        .disabled(link.route == .nearbyStage && !stage.joined)
                }
                Text(workout.status).font(.caption2)
            }.padding(.horizontal, 4)
        }
        .navigationTitle("Dance floor")
        .onAppear { workout.prepareConnection() }
        .onDisappear {
            if participating || stage.joined || stage.looking || stage.connecting { workout.stop() }
        }
    }
}
