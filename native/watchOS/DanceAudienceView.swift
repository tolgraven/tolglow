import SwiftUI
import TolglowSensorCore

struct DanceAudienceView: View {
    @ObservedObject var workout: DanceWorkout
    @ObservedObject private var link: WatchFloorLink
    @ObservedObject private var stage: AudienceClient
    @State private var sharePulse = false
    @State private var reviewingPulse = false

    init(workout: DanceWorkout) {
        self.workout = workout
        self.link = workout.link
        self.stage = workout.link.stage
    }

    private var participating: Bool { workout.active || workout.requesting }

    @ViewBuilder private func routeButton(_ route: AudienceWatchRoute, title: String, icon: String, id: String) -> some View {
        let button = Button { link.select(route) } label: {
            Label(title, systemImage: link.route == route ? "checkmark.circle.fill" : icon)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .accessibilityIdentifier(id)
        .accessibilityAddTraits(link.route == route ? .isSelected : [])
        .disabled(participating || !route.available(phoneJoined: link.phoneJoined))
        #if DEBUG && targetEnvironment(simulator)
        if WatchFloorLink.interruptedStageFixture && route == .nearbyStage {
            button.accessibilityValue(Text(link.interruptedStageDiagnostics))
        } else {
            button
        }
        #else
        button
        #endif
    }

    @ViewBuilder private var stageStatus: some View {
        #if DEBUG && targetEnvironment(simulator)
        if WatchFloorLink.interruptedStageFixture {
            Text(stage.status)
                .accessibilityIdentifier("reconnection-stage-status")
                .accessibilityValue(Text(link.interruptedStageDiagnostics))
        } else {
            Text(stage.status)
        }
        #else
        Text(stage.status)
        #endif
    }

    private var destination: some View {
        VStack(alignment: .leading, spacing: 8) {
            routeButton(.pairedPhone, title: "Paired iPhone", icon: "iphone", id: "route-paired")
            routeButton(.nearbyStage, title: "Nearby stage", icon: "antenna.radiowaves.left.and.right", id: "route-direct")
            if link.route == .nearbyStage {
                if stage.reconnecting {
                    stageStatus.accessibilityAddTraits(.updatesFrequently)
                    Button("Cancel reconnection") { workout.stop() }
                } else if stage.joined {
                    Label("Connected to stage", systemImage: "checkmark.circle")
                    if !participating { Button("Disconnect") { workout.stop() } }
                } else if let venue = link.selectedStage {
                    Label(venue.name, systemImage: "antenna.radiowaves.left.and.right")
                    if !participating { Button("Choose another stage") { link.discover() } }
                } else {
                    Text("Watch only. Leave the floor on your iPhone first.").font(.caption)
                    Button(stage.looking ? "Stop searching" : "Find stages") {
                        if stage.looking { workout.stop("Stage search stopped.") }
                        else { link.discover() }
                    }.disabled(stage.connecting)
                    ForEach(stage.venues) { venue in
                        Button(venue.name) { link.choose(venue) }
                    }
                    stageStatus.font(.caption2)
                }
            } else if !participating {
                Text(link.phoneJoined ? "Your iPhone is on the floor. Leave on iPhone to join directly." :
                     "Your Watch and iPhone share one place on the floor.").font(.caption)
            }
        }
    }

    private var pulseConsent: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 10) {
                Text("Heart rate at the stage").font(.headline)
                Text("The stage receives your BPM. A saved show can retain the last average. If only you share, that’s your BPM.")
                    .font(.caption)
                    .accessibilityIdentifier("pulse-consent")
                Text("You can stop sharing and keep dancing. The Watch discards the workout.").font(.caption)
                Button("Use heart rate") {
                    sharePulse = true
                    reviewingPulse = false
                }.accessibilityIdentifier("confirm-pulse")
                Button("Cancel", role: .cancel) { reviewingPulse = false }
            }.padding(.horizontal, 4)
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
                    } else if workout.runningWorkout {
                        Text("No recent heart rate").font(.caption)
                    }
                    if workout.runningWorkout {
                        Button("Stop sharing heart rate") {
                            sharePulse = false
                            workout.withdrawPulse()
                        }.accessibilityIdentifier("withdraw-pulse")
                    }
                } else {
                    Button {
                        if sharePulse { sharePulse = false } else { reviewingPulse = true }
                    } label: {
                        Label(sharePulse ? "Use motion only" : "Share heart rate",
                              systemImage: sharePulse ? "heart.fill" : "heart")
                    }
                    .accessibilityIdentifier("choose-pulse")
                    .disabled(workout.requesting)
                }
                Toggle("Share wrist axes", isOn: $workout.shareMotionAxes)
                if workout.active || workout.requesting {
                    Button(workout.active ? "Leave floor" : "Cancel", role: .destructive) { workout.stop() }
                } else {
                    Button(sharePulse ? "Start dance workout" : "Join with motion") { workout.start(withPulse: sharePulse) }
                        .disabled(link.route == .nearbyStage && !link.canStart)
                }
                Text(workout.status).font(.caption2)
            }.padding(.horizontal, 4)
        }
        .navigationTitle("Dance floor")
        .sheet(isPresented: $reviewingPulse) { pulseConsent }
        .onAppear {
            #if DEBUG && targetEnvironment(simulator)
            if WatchFloorLink.interruptedStageFixture {
                link.seedInterruptedStageFixture()
                return
            }
            #endif
            workout.prepareConnection()
        }
        .onDisappear {
            if participating || link.route == .nearbyStage { workout.stop() }
        }
    }
}
