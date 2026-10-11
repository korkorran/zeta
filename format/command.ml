

type info = {
  commandName: string;
  version: string;
  description: string;
}

type command = {
  info: info;
  features: Feature.featureMap;
}


