open Ocf

let gitFeatures = StringMap.empty
  |> Feature.addFeature "git_usage" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Git version control system";
        path = FeaturePath.Token.[Directive "git"];
      }
  |> Feature.addFeature "commit" {
        options = (let open Feature.CommandOption in StringMap.empty
          |> StringMap.add "--message" (Any {
              encoding = ValueNextToken (TokenEncoding.FixedString ["-m"; "--message"], (TokenEncoding.FormattedString (Some TokenEncoding.QuotedString)));
              description = "Use the given message as the commit message";
              location = "options";
            })
          |> StringMap.add "--all" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["-a"; "--all"]);
              description = "Automatically stage files that have been modified and deleted, but new files you have not told Git about are not affected";
              location = "options";
            })
          |> StringMap.add "--amend" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--amend"]);
              description = "Replace the tip of the current branch by creating a new commit";
              location = "options";
            })
        );
        anchors = (StringMap.empty
          |> StringMap.add "options" [Feature.OptionAnchor.AfterLastDirective]
        );
        args = StringMap.empty;
        description = "Record changes to the repository";
        path = FeaturePath.Token.[Directive "git"; Directive "commit"; Anchor "options"];
      }
  |> Feature.addFeature "push" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Update remote refs along with associated objects";
        path = FeaturePath.Token.[Directive "git"; Directive "push"];
      }
  |> Feature.addFeature "pull" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Fetch from and integrate with another repository or a local branch";
        path = FeaturePath.Token.[Directive "git"; Directive "pull"];
      }
  |> Feature.addFeature "status" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Show the working tree status";
        path = FeaturePath.Token.[Directive "git"; Directive "status"];
      }
  |> Feature.addFeature "clone" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Clone a repository into a new directory";
        path = FeaturePath.Token.[Directive "git"; Directive "clone"];
      }
  |> Feature.addFeature "list_branch" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "List branches";
        path = FeaturePath.Token.[Directive "git"; Directive "branch"];
      }
  |> Feature.addFeature "create_branch" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = (let open Feature.Argument in StringMap.empty
          |> StringMap.add "branch_name" (Any {encoding = Single (FormattedString (Some SingleWord)); description = "Name of the branch to create"})
        );
        description = "Create a new branch";
        path = FeaturePath.Token.[Directive "git"; Directive "branch"; ArgumentLocation "branch_name"];
      }
  |> Feature.addFeature "checkout" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = StringMap.empty;
        description = "Switch branches or restore working tree files";
        path = FeaturePath.Token.[Directive "git"; Directive "checkout"];
      }
  |> Feature.addFeature "get_config" {
        options = (let open Feature.CommandOption in StringMap.empty
          |> StringMap.add "--global" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--global"]);
              description = "Use global config file";
              location = "options";
            })
          |> StringMap.add "--system" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--system"]);
              description = "Use system config file";
              location = "options";
            })
          |> StringMap.add "--local" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--local"]);
              description = "Use local config file";
              location = "options";
            })
          |> StringMap.add "--list" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--list"]);
              description = "List all config settings";
              location = "options";
            })
        );
        anchors = (StringMap.empty
          |> StringMap.add "options" []
        );
        args = StringMap.empty;
        description = "Get repository or global options";
        path = FeaturePath.Token.[Directive "git"; Directive "config"; Anchor "options"];
      }
  |> Feature.addFeature "set_config" {
        options = StringMap.empty;
        anchors = StringMap.empty;
        args = (let open Feature.Argument in StringMap.empty
          |> StringMap.add "key" (Any {encoding = Single (FormattedString (Some SingleWord)); description = "Name of the config key"})
          |> StringMap.add "value" (Any {encoding = Single (FormattedString (Some SingleWord)); description = "Value to set for the config key"})
        );
        description = "Set a config value";
        path = FeaturePath.Token.[Directive "git"; Directive "config"; ArgumentLocation "key"; ArgumentLocation "value"];
      }
  |> Feature.addFeature "merge" {
        options = (let open Feature.CommandOption in StringMap.empty
          |> StringMap.add "--no-ff" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-ff"]);
              description = "Create a merge commit even when the merge resolves as a fast-forward";
              location = "options";
            })
          |> StringMap.add "--ff-only" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--ff-only"]);
              description = "Refuse to merge and exit with a non-zero status unless the current HEAD is already up to date or the merge can be resolved as a fast-forward";
              location = "options";
            })
          |> StringMap.add "--squash" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--squash"]);
              description = "Produce the working tree and index state as if a real merge happened (except no commit is made), but do not actually make a commit or move the HEAD, and pretend as if the merge failed to allow further manipulation (e.g. by squashing the resulting changes into another commit)";
              location = "options";
            })
          |> StringMap.add "--no-squash" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-squash"]);
              description = "Perform a normal merge (the default)";
              location = "options";
            })
          |> StringMap.add "--commit" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--commit"]);
              description = "Perform a merge and commit the result (the default)";
              location = "options";
            })
          |> StringMap.add "--no-commit" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-commit"]);
              description = "Perform a merge and do not commit the result";
              location = "options";
            })
          |> StringMap.add "--edit" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--edit"; "-e"]);
              description = "Invoke the editor before committing the resulting merge to edit the auto-generated merge message";
              location = "options";
            })
          |> StringMap.add "--no-edit" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-edit"]);
              description = "Use the auto-generated merge message without launching an editor";
              location = "options";
            })
          |> StringMap.add "--abort" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--abort"]);
              description = "Abort the merge and try to reconstruct the pre-merge state";
              location = "options";
            })
          |> StringMap.add "--continue" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--continue"]);
              description = "Continue the merge after having resolved any conflicts";
              location = "options";
            })
          |> StringMap.add "--quit" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--quit"]);
              description = "Abort the merge and keep the working tree as it is";
              location = "options";
            })
          |> StringMap.add "--no-verify" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-verify"]);
              description = "Bypass the pre-merge and commit-msg hooks";
              location = "options";
            })
          |> StringMap.add "--verify" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--verify"]);
              description = "Run the pre-merge and commit-msg hooks (the default)";
              location = "options";
            })
          |> StringMap.add "--verify-signatures" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--verify-signatures"]);
              description = "Verify the GPG signature of the commits being merged";
              location = "options";
            })
          |> StringMap.add "--no-verify-signatures" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-verify-signatures"]);
              description = "Do not verify the GPG signature of the commits being merged (the default)";
              location = "options";
            })
          |> StringMap.add "--signoff" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--signoff"; "-s"]);
              description = "Add Signed-off-by line by the committer at the end of the commit log message";
              location = "options";
            })
          |> StringMap.add "--no-signoff" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-signoff"]);
              description = "Do not add Signed-off-by line by the committer at the end of the commit log message (the default)";
              location = "options";
            })
          |> StringMap.add "--gpg-sign" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--gpg-sign"; "-S"]);
              description = "GPG-sign the commits being merged";
              location = "options";
            })
          |> StringMap.add "--no-gpg-sign" (Any {
              encoding = SingleToken (TokenEncoding.FixedString ["--no-gpg-sign"]);
              description = "Do not GPG-sign the commits being merged (the default)";
              location = "options";
            })
          |> StringMap.add "--strategy" (Any {
              encoding = ValueNextToken (TokenEncoding.FixedString ["--strategy"], Enum ["resolve"; "recursive"; "octopus"; "ours"; "subtree"]);
              description = "Use the given merge strategy";
              location = "options";
            })
          );
        anchors = (StringMap.empty
          |> StringMap.add "options" []
        );
        args = (StringMap.empty
          |> StringMap.add "branch-name" (let open Feature.Argument in Any {encoding = Single (FormattedString (Some SingleWord)); description = "Name of the branch to merge"})
        );
        description = "Join two or more development histories together";
        path = FeaturePath.Token.[Directive "git"; Directive "merge"; Anchor "options"; ArgumentLocation "branch-name"];
      }


let git : Ocf.Command.command = {
  info = {
    commandName = "git";
    version = "2.40.0";
    description = "Git version control system";
  };
  features = gitFeatures;
}
