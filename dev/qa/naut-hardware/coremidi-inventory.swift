// Read-only CoreMIDI inventory for the Naut hardware kit.
// It creates no MIDI client and opens no port: it only reads object properties
// from the MIDI server, so it cannot send to or reconfigure any device.
// Run: swift dev/qa/naut-hardware/coremidi-inventory.swift [filter]
import CoreMIDI
import Foundation

func str(_ obj: MIDIObjectRef, _ key: CFString) -> String? {
    var value: Unmanaged<CFString>?
    guard MIDIObjectGetStringProperty(obj, key, &value) == noErr, let v = value else { return nil }
    return v.takeRetainedValue() as String
}

func int(_ obj: MIDIObjectRef, _ key: CFString) -> Int32? {
    var value: Int32 = 0
    return MIDIObjectGetIntegerProperty(obj, key, &value) == noErr ? value : nil
}

func edn(_ s: String?) -> String {
    guard let s = s else { return "nil" }
    return "\"" + s.replacingOccurrences(of: "\\", with: "\\\\").replacingOccurrences(of: "\"", with: "\\\"") + "\""
}

func endpoint(_ e: MIDIEndpointRef, _ direction: String) -> String {
    "{:direction :\(direction) :name \(edn(str(e, kMIDIPropertyName))) :display-name \(edn(str(e, kMIDIPropertyDisplayName)))"
        + " :unique-id \(int(e, kMIDIPropertyUniqueID).map(String.init) ?? "nil") :offline? \((int(e, kMIDIPropertyOffline) ?? 0) != 0)}"
}

let filter = CommandLine.arguments.dropFirst().first?.lowercased()
var lines: [String] = []
for d in 0..<MIDIGetNumberOfDevices() {
    let device = MIDIGetDevice(d)
    let name = str(device, kMIDIPropertyName) ?? ""
    if let f = filter, !name.lowercased().contains(f) { continue }
    var endpoints: [String] = []
    for i in 0..<MIDIDeviceGetNumberOfEntities(device) {
        let entity = MIDIDeviceGetEntity(device, i)
        for s in 0..<MIDIEntityGetNumberOfSources(entity) { endpoints.append(endpoint(MIDIEntityGetSource(entity, s), "source")) }
        for s in 0..<MIDIEntityGetNumberOfDestinations(entity) { endpoints.append(endpoint(MIDIEntityGetDestination(entity, s), "destination")) }
    }
    lines.append("{:device \(edn(name)) :manufacturer \(edn(str(device, kMIDIPropertyManufacturer))) :model \(edn(str(device, kMIDIPropertyModel)))"
        + " :driver \(edn(str(device, kMIDIPropertyDriverOwner))) :unique-id \(int(device, kMIDIPropertyUniqueID).map(String.init) ?? "nil")"
        + " :offline? \((int(device, kMIDIPropertyOffline) ?? 0) != 0)\n :endpoints [" + endpoints.joined(separator: "\n             ") + "]}")
}
print("[" + lines.joined(separator: "\n ") + "]")
