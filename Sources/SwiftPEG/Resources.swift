import Foundation

public enum Resources {
    public static var resources: Bundle {
        Bundle.module
    }

    public static var metagrammarFile: URL {
        guard let url = resources.url(forResource: "metagrammar", withExtension: "gram") else {
            fatalError("Failed to find embedded metagrammar.gram file")
        }
        
        return url
    }
}
