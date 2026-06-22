package cs305.minesweeper;

import java.awt.Dimension;

/**
 * A representation of a board in the game Minesweeper
 * 
 * @author Michael Sergio
 */
public interface MinesweeperBoard {

	/**
	 * State of the Game
	 */
	enum GameState {
		/**
		 * A pre-game state
		 */
		Waiting,
		/**
		 * The game during it's play state
		 */
		Playing,
		/**
		 * A post-game state representing a loss
		 */
		Lost,
		/**
		 * A post-game lost representing a win
		 */
		Won
	};

	/**
	 * Gets the size of the board.
	 * For example, a 1 X 1 board will return [1,1], a 2 x 2 board will return [2,2]
	 * @return A dimension containing: [The length of X, The length of Y]
	 */
	Dimension getBoardSize();

	/**
	 * Gets the state of the game
	 * @return the game state
	 */
	GameState getGameState();

	/**
	 * Gets a tile at a particular position on the board
	 * @param x The 0-based X position
	 * @param y The 0-based Y position
	 * @return A MinesweeperTile at [x,y]
	 */
	MinesweeperTile getTile(int x, int y);

	/**
	 * Gets an array containing a references to all tiles on the board
	 * @return All the tiles on the board
	 */
	MinesweeperTile[] getAllTiles();

	/**
	 * Gets an array containing references to all the hidden tiles on the board.
	 * @return All the hidden tiles on the board
	 */
	MinesweeperTile[] getHiddenTiles();

	/**
	 * returns all the Tiles that are exposed and have a value range between 0 an 9
	 * not including 0 or 9
	 * @return All exposed tiles that have a number value
	 */
	MinesweeperTile[] getNumberTiles();
	
	/**
	 * displays the board of the game
	 */
	void displayBoard();
	
	
	/**
	 * resets the board for a new game
	 */
	void resetBoard();

	/**
	 * Flags a tile on the board.
	 * Flagging means the user thinks there is a bomb at that spot.
	 * Flagging a tile will marked the tile as flagged.
	 * @param tile A valid reference to a title on the board
	 */
	void flagTile(MinesweeperTile tile);

	/**
	 * Flags a tile on the board.
	 * Flagging means the user thinks there is a bomb at that spot.
	 * Flagging a tile will marked the tile as flagged.
	 * @param x A valid X position between 0 and size - 1
	 * @param y A valid Y position between 0 and size - 1
	 */
	void flagTile(int x, int y);

	/**
	 * Clicks a tile on the board.
	 * Clicking means the user thinks there is no bomb at that spot.
	 * Clicking a title exposes the tile and may change the game state if it is a bomb.
	 * @param tile A valid reference to a title on the board
	 */
	void clickTile(MinesweeperTile tile);

	/**
	 * Clicks a tile on the board.
	 * Clicking means the user thinks there is no bomb at that spot.
	 * Clicking a title exposes the tile and may change the game state if it is a bomb.
	 * @param x A valid X position between 0 and size - 1
	 * @param y A valid Y position between 0 and size - 1
	 */
	void clickTile(int x, int y);

}