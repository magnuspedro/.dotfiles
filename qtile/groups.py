from libqtile.config import Group, Key
from libqtile.lazy import lazy
from env import group_names, mod


groups = [Group(name, **kwargs) for name, kwargs in group_names]

def create_groups(keys):

    for i, (name, kwargs) in enumerate(group_names, 1):
        keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
        keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

