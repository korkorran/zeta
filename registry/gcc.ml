open Ocf

let gccFeatures = StringMap.empty
  |> Feature.addFeature "compile" {
    options = (let open Feature.CommandOption in StringMap.empty
      |> StringMap.add "include_dir" (Any {
          encoding = SingleToken (TokenEncoding.Prefixed (["-I"], TokenEncoding.FormattedString (Some TokenEncoding.FilePath)));
          description = "Add the directory to the list of directories searched for header files";
          location = "options";
        })
      |> StringMap.add "library_dir" (Any {
          encoding = SingleToken (TokenEncoding.Prefixed (["-L"], TokenEncoding.FormattedString (Some TokenEncoding.FilePath)));
          description = "Add the directory to the list of directories searched for libraries";
          location = "options";
        })
      |> StringMap.add "library" (Any {
          encoding = SingleToken (TokenEncoding.Prefixed (["-l"], TokenEncoding.FormattedString (Some TokenEncoding.SingleWord)));
          description = "Link with the library";
          location = "options";
        })
      |> StringMap.add "output" (Any {
          encoding = ValueNextToken (TokenEncoding.FixedString ["-o"], TokenEncoding.FormattedString (Some TokenEncoding.FilePath));
          description = "Place the output in the file";
          location = "options";
        })
    );
    anchors = StringMap.empty
      |> StringMap.add "options" [Feature.OptionAnchor.AfterLastDirective];
    args = StringMap.empty
      |> StringMap.add "sources" (Feature.Argument.Any { encoding = Feature.Argument.Over (1, TokenEncoding.FormattedString (Some TokenEncoding.FilePath)); description = "Source files to compile" });
    description = "Compile and link source files";
    path = FeaturePath.Token.[Directive "gcc"; Anchor "options"; ArgumentLocation "sources"];
  }

let gcc : Ocf.Command.command = {
  info = {
    commandName = "gcc";
    version = "UNVERSIONED";
    description = "GNU project C and C++ compiler";
  };
  features = gccFeatures;
}
