open Mapping.Command
open Mapping.Typeformat
let git = emptyCommand
  |> addPath "git" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Git version control system";
      }  
  |> addPath "git commit" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Record changes to the repository";
      }
  |> addPath "git push" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Update remote refs along with associated objects";
      }
  |> addPath "git pull" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Fetch from and integrate with another repository or a local branch";
      }
  |> addPath "git status" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Show the working tree status";
      }
  |> addPath "git clone" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Clone a repository into a new directory";
      }
  |> addPath "git branch" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "List branches";
      }
  |> addPath "git branch <branch-name>" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = (let open Argument in StringMap.empty
          |> StringMap.add "branch-name" {encoding = SingleString; description = "Name of the branch to create"}
        );
        description = "Create a new branch";
      }
  |> addPath "git checkout" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Switch branches or restore working tree files";
      }
  |> addPath "git config [?options]" {
        options = (let open CommandOption in StringMap.empty
          |> StringMap.add "--global" {
              encoding = SingleString ["--global"];
              description = "Use global config file";
              location = "options";
            }
          |> StringMap.add "--system" {
              encoding = SingleString ["--system"];
              description = "Use system config file";
              location = "options";
            }
          |> StringMap.add "--local" {
              encoding = SingleString ["--local"];
              description = "Use local config file";
              location = "options";
            }
          |> StringMap.add "--list" {
              encoding = SingleString ["--list"];
              description = "List all config settings";
              location = "options";
            }
        );
        optionsLocations = (StringMap.empty
          |> StringMap.add "options" {from=1; to_=100}
        );
        args = StringMap.empty;
        description = "Get and set repository or global options";
      }
  |> addPath "git config <key> <value>" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = (let open Argument in StringMap.empty
          |> StringMap.add "key" {encoding = SingleString; description = "Name of the config key"}
          |> StringMap.add "value" {encoding = SingleString; description = "Value to set for the config key"}
        );
        description = "Set a config value";
      }
  |> addPath "git merge [?options] <branch-name>" {
        options = (let open CommandOption in StringMap.empty 
          |> StringMap.add "--no-ff" {
              encoding = SingleString ["--no-ff"];
              description = "Create a merge commit even when the merge resolves as a fast-forward";
              location = "options";
            }
          |> StringMap.add "--ff-only" {
              encoding = SingleString ["--ff-only"];
              description = "Refuse to merge and exit with a non-zero status unless the current HEAD is already up to date or the merge can be resolved as a fast-forward";
              location = "options";
            }
          |> StringMap.add "--squash" {
              encoding = SingleString ["--squash"];
              description = "Produce the working tree and index state as if a real merge happened (except no commit is made), but do not actually make a commit or move the HEAD, and pretend as if the merge failed to allow further manipulation (e.g. by squashing the resulting changes into another commit)";
              location = "options";
            }
          |> StringMap.add "--no-squash" {
              encoding = SingleString ["--no-squash"];
              description = "Perform a normal merge (the default)";
              location = "options";
            }
          |> StringMap.add "--commit" {
              encoding = SingleString ["--commit"];
              description = "Perform a merge and commit the result (the default)";
              location = "options";
            }
          |> StringMap.add "--no-commit" {
              encoding = SingleString ["--no-commit"];
              description = "Perform a merge and do not commit the result";
              location = "options";
            }
          |> StringMap.add "--edit" {
              encoding = SingleString ["--edit"; "-e"];
              description = "Invoke the editor before committing the resulting merge to edit the auto-generated merge message";
              location = "options";
            }
          |> StringMap.add "--no-edit" {
              encoding = SingleString ["--no-edit"];
              description = "Use the auto-generated merge message without launching an editor";
              location = "options";
            }
          |> StringMap.add "--abort" {
              encoding = SingleString ["--abort"];
              description = "Abort the merge and try to reconstruct the pre-merge state";
              location = "options";
            }
          |> StringMap.add "--continue" {
              encoding = SingleString ["--continue"];
              description = "Continue the merge after having resolved any conflicts";
              location = "options";
            }
          |> StringMap.add "--quit" {
              encoding = SingleString ["--quit"];
              description = "Abort the merge and keep the working tree as it is";
              location = "options";
            }
          |> StringMap.add "--no-verify" {
              encoding = SingleString ["--no-verify"];
              description = "Bypass the pre-merge and commit-msg hooks";
              location = "options";
            }
          |> StringMap.add "--verify" {
              encoding = SingleString ["--verify"];
              description = "Run the pre-merge and commit-msg hooks (the default)";
              location = "options";
            }
          |> StringMap.add "--verify-signatures" {
              encoding = SingleString ["--verify-signatures"];
              description = "Verify the GPG signature of the commits being merged";
              location = "options";
            }
          |> StringMap.add "--no-verify-signatures" {
              encoding = SingleString ["--no-verify-signatures"];
              description = "Do not verify the GPG signature of the commits being merged (the default)";
              location = "options";
            }
          |> StringMap.add "--signoff" {
              encoding = SingleString ["--signoff"; "-s"];
              description = "Add Signed-off-by line by the committer at the end of the commit log message";
              location = "options";
            }
          |> StringMap.add "--no-signoff" {
              encoding = SingleString ["--no-signoff"];
              description = "Do not add Signed-off-by line by the committer at the end of the commit log message (the default)";
              location = "options";
            }
          |> StringMap.add "--gpg-sign" {
              encoding = SingleString ["--gpg-sign"; "-S"];
              description = "GPG-sign the commits being merged";
              location = "options";
            }
          |> StringMap.add "--no-gpg-sign" {
              encoding = SingleString ["--no-gpg-sign"];
              description = "Do not GPG-sign the commits being merged (the default)";
              location = "options";
            }
          |> StringMap.add "--strategy" {
              encoding = ValueSuffix (["--strategy"], TEnum ["resolve"; "recursive"; "octopus"; "ours"; "subtree"]);
              description = "Use the given merge strategy";
              location = "options";
            }
          );
        optionsLocations = (StringMap.empty
          |> StringMap.add "options" {from=1; to_=100}
        );
        args = (StringMap.empty
          |> StringMap.add "branch-name" (let open Argument in {encoding = SingleString; description = "Name of the branch to merge"})
        );
        description = "Join two or more development histories together";
      }
