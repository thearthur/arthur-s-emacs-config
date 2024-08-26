{
  description = "emacs config";

  outputs = { self }: {
    # Expose the init.el file directly
    defaultPackage = self: {
      src = ./init.el;
    };
  };
}
