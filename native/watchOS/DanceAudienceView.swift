import SwiftUI

struct DanceAudienceView: View {
    @ObservedObject var workout: DanceWorkout
    @State private var sharePulse = false

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 10) {
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
                }
                Text(workout.status).font(.caption2)
            }.padding(.horizontal, 4)
        }
        .navigationTitle("Dance floor")
        .onDisappear { if workout.active || workout.requesting { workout.stop() } }
    }
}
