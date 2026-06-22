package cs305.minesweeper;

/**
 * A representation of a tile in the game Minesweeper
 * 
 * @author Michael Sergio
 */
public interface MinesweeperTile {

	/**
	 * A state a tile may be in. 
	 * 
	 */
	enum TileState {
		/**
		 * A tile is hidden when it is not visible by player and has not been flagged.
		 * A hidden tile's value is the constant "UNKNOWN_VALUE"
		 */
		Hidden,
		/**
		 * A tile is exposed when it has been clicked on by a user.
		 * A tiles value when exposed may be either:
		 * 		 An integer between 0-9 or one of the constant's "BOMB_VALUE, EMPTY_VALUE"
		 */
		Exposed,
		/**
		 * A tile is flagged when it is flagged by the user.
		 * A flagged tiles value will be "FLAG_VALUE"
		 */
		Flagged
	};

	/**
	 * Gets a tile's state
	 * @return the state of the current tile
	 */
	TileState getState();

	/**
	 * Returns non-null references to tiles surrounding the current tile.
	 * The references are the centered around the current tile in a 3 x 3 style excluding this tile.
	 * If tile is against a wall 
	 * @return all surronding tiles
	 */
	MinesweeperTile[] getSurroundingTiles();

	int BOMB_VALUE = 0xBAD;
	int FLAG_VALUE = Integer.MIN_VALUE + 1;
	int UNKNOWN_VALUE = "Sergio".hashCode(); // Ox9366E38D
	int EMPTY_VALUE = 0;

	int getValue();
}