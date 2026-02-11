open Mapping.Command
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
        description = "List, create, or delete branches";
      }
  |> addPath "git checkout" {
        options = StringMap.empty;
        optionsLocations = StringMap.empty;
        args = StringMap.empty;
        description = "Switch branches or restore working tree files";
      }
  |> addPath "git config [?options]" {
        options = (StringMap.empty
          |> StringMap.add "--global" {
              encodings = ["--global"];
              description = "Use global config file";
              location = "options";
            }
          |> StringMap.add "--system" {
              encodings = ["--system"];
              description = "Use system config file";
              location = "options";
            }
          |> StringMap.add "--local" {
              encodings = ["--local"];
              description = "Use local config file";
              location = "options";
            }
          |> StringMap.add "--list" {
              encodings = ["--list"];
              description = "List all config settings";
              location = "options";
            }
        );
        optionsLocations = (StringMap.empty
          |> StringMap.add "options" {from=1; to_=10}
        );
        args = StringMap.empty;
        description = "Get and set repository or global options";
      }
