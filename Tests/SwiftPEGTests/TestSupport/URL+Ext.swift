import Foundation

extension URL {
    func test_relativePath(to basePath: URL) -> String {
        return test_relativeURL(to: basePath).relativePath
    }

    func test_relativeURL(to basePath: URL) -> URL {
        let base = basePath.pathComponents
        let path = self.pathComponents
        let minLength = min(base.count, path.count)

        var index = 0
        while index < minLength && base[index] == path[index] {
            index += 1
        }

        if index >= path.count {
            return self
        }

        let remaining = path[index...]
        let finalUrl = remaining.reduce(URL(fileURLWithPath: "", relativeTo: basePath)) { (url, component) in
            url.appendingPathComponent(component)
        }

        return finalUrl
    }
}
