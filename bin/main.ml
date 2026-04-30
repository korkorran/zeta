
type commit_options = {
  message : string Lib.commandOption
}
type commit_arguments = unit

type create_branch_options = unit
type create_branch_arguments = {
  branch_name : string Lib.commandArg
}

type git_mapping = {
  commit : (commit_options, commit_arguments) Lib.feature;
  create_branch : (create_branch_options, create_branch_arguments) Lib.feature;
}
[@@deriving count_fields]

let git_mapping : git_mapping = {
  commit = {
    path = "git commit [?options]";
    options = {
      message = {
        encoding = ValueSuffix (["-m"; "--message"], Lib.SimpleString);
        description = "Use the given message as the commit message";
        location = "options";
      }
    };
    args = ();
    description = "Record changes to the repository";
  };
  create_branch = {
    path = "git branch <branch_name>";
    options = ();
    args = {
      branch_name = {
        encoding = Single (Lib.SimpleString);
        description = "Name of the branch to create";
      }
    };
    description = "Create a new branch";
  };
}

let () = ignore git_mapping; ignore git_mapping.commit; ignore git_mapping.commit.options; ignore git_mapping.commit.options.message; ignore git_mapping.commit.options.message.encoding; ignore git_mapping.commit.options.message.description; ignore git_mapping.commit.options.message.location; ignore git_mapping.commit.path; ignore git_mapping.commit.description; ignore git_mapping.commit.args
let () = ignore git_mapping.create_branch; ignore git_mapping.create_branch.options; ignore git_mapping.create_branch.args; ignore git_mapping.create_branch.args.branch_name; ignore git_mapping.create_branch.args.branch_name.encoding; ignore git_mapping.create_branch.args.branch_name.description; ignore git_mapping.create_branch.path; ignore git_mapping.create_branch.description

let () = print_endline ("Number of fields in commit options: " ^ string_of_int git_mapping_field_count);